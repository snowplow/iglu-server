/*
 * Copyright (c) 2019-2021 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at
 * http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and
 * limitations there under.
 */
package com.snowplowanalytics.iglu.server.service

import io.circe.Json

import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.rho.{AuthedContext, RhoMiddleware, RhoRoutes}
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.rho.swagger.syntax.io

import cats.Id
import cats.data.{EitherT, NonEmptyList, Validated, ValidatedNel}
import cats.effect.{IO, Sync}
import cats.implicits._

import com.snowplowanalytics.iglu.core.{SchemaMap, SchemaVer, SelfDescribingData, SelfDescribingSchema, VersionKind}
import com.snowplowanalytics.iglu.core.circe.implicits._

import com.snowplowanalytics.iglu.client.validator.ValidatorError
import com.snowplowanalytics.iglu.client.validator.CirceValidator
import com.snowplowanalytics.iglu.schemaddl.IgluSchema
import com.snowplowanalytics.iglu.schemaddl.experimental.Bumps
import com.snowplowanalytics.iglu.schemaddl.jsonschema.circe.implicits._
import com.snowplowanalytics.iglu.schemaddl.jsonschema.{Pointer, SelfSyntaxChecker, Schema => SchemaAst}
import com.snowplowanalytics.iglu.schemaddl.jsonschema.SanityLinter.lint
import com.snowplowanalytics.iglu.schemaddl.jsonschema.Linter.{Level, Message, allLintersMap}
import com.snowplowanalytics.iglu.schemaddl.migrations.{SchemaDiff, SchemaList}

import com.snowplowanalytics.iglu.server.storage.Storage
import com.snowplowanalytics.iglu.server.middleware.PermissionMiddleware
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Permission, Schema, VersionCursor}
import com.snowplowanalytics.iglu.server.codecs.JsonCodecs._
import com.snowplowanalytics.iglu.server.codecs.UriParsers._

class ValidationService[F[+_]: Sync](
  swagger: SwaggerSyntax[F],
  ctx: AuthedContext[F, Permission],
  db: Storage[F],
  patchesAllowed: Boolean
) extends RhoRoutes[F] {
  import swagger._
  import ValidationService._

  val schemaFormat = pathVar[Schema.Format]("format", "Schema format, e.g. jsonschema")

  "This route allows you to validate schemas" **
    POST / "validate" / "schema" / schemaFormat >>> ctx.auth ^ jsonDecoder[F] |>> validateSchema _

  "This route allows you to validate self-describing instances" **
    POST / "validate" / "instance" >>> ctx.auth ^ jsonDecoder[F] |>> validateData _

  def validateSchema(format: Schema.Format, permission: Permission, schema: Json) =
    format match {
      case Schema.Format.Jsonschema =>
        validateJsonSchema(schema, db, permission, patchesAllowed).flatMap {
          case Validated.Valid(sd) =>
            val message = s"The schema provided is a valid self-describing ${sd.self.schemaKey.toSchemaUri} schema"
            Ok(IgluResponse.Message(message): IgluResponse)
          case Validated.Invalid(report) =>
            Ok(IgluResponse.SchemaValidationReport(report): IgluResponse)
        }
    }

  def validateData(authInfo: Permission, instance: Json) =
    SelfDescribingData.parse(instance) match {
      case Right(SelfDescribingData(key, data)) =>
        for {
          schema <- db.getSchema(SchemaMap(key))
          response <- schema match {
            case Some(Schema(_, meta, schemaBody)) if meta.isPublic || authInfo.canRead(key.vendor) =>
              CirceValidator.validate(data, schemaBody) match {
                case Left(ValidatorError.InvalidData(report)) =>
                  Ok(IgluResponse.InstanceValidationReport(report): IgluResponse)
                case Left(ValidatorError.InvalidSchema(_)) =>
                  val message = s"Schema ${key.toSchemaUri} fetched from DB is invalid"
                  InternalServerError(IgluResponse.Message(message): IgluResponse)
                case Right(_) =>
                  Ok(IgluResponse.Message(s"Instance is valid ${key.toSchemaUri}"): IgluResponse)
              }
            case _ =>
              NotFound(IgluResponse.SchemaNotFound: IgluResponse)
          }
        } yield response
      case Left(error) =>
        BadRequest(IgluResponse.Message(s"JSON payload is not self-describing, ${error.code}"): IgluResponse)
    }

}

object ValidationService {

  def asRoutes(
    patchesAllowed: Boolean
  )(
    db: Storage[IO],
    ctx: AuthedContext[IO, Permission],
    rhoMiddleware: RhoMiddleware[IO]
  ): HttpRoutes[IO] = {
    val service = new ValidationService[IO](io, ctx, db, patchesAllowed).toRoutes(rhoMiddleware)
    PermissionMiddleware.wrapService(db, ctx, service)
  }

  type LintReport[A] = ValidatedNel[Message, A]

  val NotSelfDescribing = Message(Pointer.Root, "JSON Schema is not self-describing", Level.Error)
  val NotSchema         = Message(Pointer.Root, "Cannot extract JSON Schema", Level.Error)

  def validateJsonSchema[F[_]: Sync](
    schema: Json,
    db: Storage[F],
    permission: Permission,
    patchesAllowed: Boolean
  ): F[LintReport[SelfDescribingSchema[Json]]] = {

    val selfDescribing = SelfDescribingSchema.parse(schema)
    val schemaAst      = SchemaAst.parse(schema)
    val igluSchema = (selfDescribing.toOption, schemaAst).mapN {
      case (s, ast) => SelfDescribingSchema(s.self, ast)
    }

    val generalCheck =
      SelfSyntaxChecker.validateSchema(schema)

    val selfDescribingCheck =
      selfDescribing.fold(_ => NotSelfDescribing.invalidNel[SelfDescribingSchema[Json]], _.validNel[Message])

    val lintReport: LintReport[Unit] =
      schemaAst.fold(NotSchema.invalidNel[SchemaAst])(_.validNel[Message]).andThen { ast =>
        val result = lint(ast, allLintersMap.values.toList).toList.flatMap {
          case (pointer, issues) => issues.toList.map(_.toMessage(pointer))
        }
        NonEmptyList.fromList(result).fold(().validNel[Message])(_.invalid[Unit])
      }

    getExistingVersions(db, permission, selfDescribing.toOption).map { existing =>
      val schemaVerCheck =
        selfDescribing.map(schemaVerIsAllowed(existing, patchesAllowed)).getOrElse(Right(())).toValidatedNel
      val schemaBumpCheck = igluSchema.map(schemaBumpIsAllowed(existing)).getOrElse(Right(())).toValidatedNel

      (generalCheck, lintReport, schemaVerCheck, schemaBumpCheck, selfDescribingCheck).mapN((_, _, _, _, schema) =>
        schema
      )

    }
  }

  private def getExistingVersions[F[_]: Sync](
    db: Storage[F],
    permission: Permission,
    schemaOpt: Option[SelfDescribingSchema[Json]]
  ): F[List[Schema]] =
    schemaOpt match {
      case Some(schema) =>
        db.getSchemasByVendorName(schema.self.schemaKey.vendor, schema.self.schemaKey.name).compile.toList.map {
          existing =>
            if (existing.forall(s => permission.canRead(s.schemaMap.schemaKey.vendor) || s.metadata.isPublic)) existing
            else Nil
        }
      case None => List.empty[Schema].pure[F]
    }

  private def schemaVerIsAllowed(existing: List[Schema], patchesAllowed: Boolean)(
    schema: SelfDescribingSchema[Json]
  ): Either[Message, Unit] = {
    val versions  = existing.map(_.schemaMap.schemaKey.version)
    val schemaVer = schema.self.schemaKey.version.asString
    VersionCursor.isAllowed(schema.self.schemaKey.version, versions, false).map(_ => ()).leftFlatMap {
      case VersionCursor.Inconsistency.PreviousMissing =>
        Message(Pointer.Root, s"The SchemaVer preceding $schemaVer is missing in the database", Level.Warning).asLeft
      case VersionCursor.Inconsistency.AlreadyExists =>
        val msg =
          if (patchesAllowed) s"Schema $schemaVer already exists and would be overwritten"
          else s"Schema $schemaVer already exists"
        Message(Pointer.Root, msg, Level.Warning).asLeft
      case VersionCursor.Inconsistency.Availability(_, _) =>
        Right(())
    }
  }

  private def toSchemaList(schemas: NonEmptyList[IgluSchema]): SchemaList =
    SchemaList.fromFetchedSchemas[Id, Nothing](EitherT.rightT[Id, Nothing](schemas)).value match {
      case Right(schemaLists)                         => schemaLists.head
      case _: Left[Nothing, NonEmptyList[SchemaList]] => throw new IllegalStateException()
    }

  private def schemaBumpIsAllowed(existing: List[Schema])(schema: IgluSchema): Either[Message, Unit] =
    existing.traverse { s =>
      SchemaAst.parse(s.canonical.schema).map(ast => SelfDescribingSchema(s.canonical.self, ast))
    } match {
      case None =>
        // This is an illegal state: we should always be able to parse database schemas
        Right(())
      case Some(parsed) =>
        val allowed = findValidVersionBumps(schema, parsed)
        val v       = schema.self.schemaKey.version
        if (allowed.toList.contains(v)) {
          Right(())
        } else {
          val allowedStr = allowed.map(_.asString).toList.mkString(", ")
          val bumpType = VersionCursor.get(v) match {
            case VersionCursor.Initial             => "initial version"
            case VersionCursor.StartModel(_)       => "model increment"
            case VersionCursor.StartRevision(_, _) => "revision increment"
            case VersionCursor.NonInitial(_)       => "addition increment"
          }
          Message(
            Pointer.Root,
            s"Schema change is not a valid $bumpType. Suggested versions: $allowedStr",
            Level.Warning
          ).asLeft
        }
    }

  private def findValidVersionBumps(schema: IgluSchema, existing: List[IgluSchema]): NonEmptyList[SchemaVer.Full] = {
    val byModel = existing.groupByNel(_.self.schemaKey.version.model)
    val modelBump = byModel.keys.toList.sorted.lastOption match {
      case None        => SchemaVer.Full(1, 0, 0)
      case Some(model) => SchemaVer.Full(model + 1, 0, 0)
    }
    val others = byModel.toList.flatMap {
      case (model, nel) =>
        val lastRevision = nel.map(_.self.schemaKey.version.revision).sorted.last
        val revisionBump =
          toSchemaList(nel.append(schema)) match {
            case SchemaList.Single(_) => None
            case full @ SchemaList.Full(_) =>
              val diff = SchemaDiff.build(full.toSegment)
              Bumps.getPointer(diff) match {
                case Some(VersionKind.Revision | VersionKind.Addition) =>
                  Some(SchemaVer.Full(model, lastRevision + 1, 0))
                case _ => None
              }
          }

        val additions = nel.groupBy(_.self.schemaKey.version.revision).toList.flatMap {
          case (revision, nel) =>
            toSchemaList(nel.append(schema)) match {
              case SchemaList.Single(_) => None
              case full @ SchemaList.Full(_) =>
                val diff = SchemaDiff.build(full.toSegment)
                Bumps.getPointer(diff) match {
                  case Some(VersionKind.Addition) =>
                    val lastAddition = nel.map(_.self.schemaKey.version.addition).sorted.last
                    Some(SchemaVer.Full(model, revision, lastAddition + 1))
                  case _ => None
                }
            }
        }

        revisionBump.toList ::: additions
    }

    NonEmptyList(modelBump, others)
  }
}
