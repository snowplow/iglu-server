/*
 * Copyright (c) 2019-2023 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.server
package service

import java.util.UUID

import cats.data.{EitherT, Validated}
import cats.effect._
import cats.implicits._

import io.circe.Json

import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.rho.bits.TextMetaData
import org.http4s.rho.{AuthedContext, RhoMiddleware, RhoRoutes}
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.rho.swagger.syntax.{io => swaggerSyntax}

import com.snowplowanalytics.iglu.core.{SchemaMap, SchemaVer, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.implicits._

import com.snowplowanalytics.iglu.schemaddl.jsonschema.Linter
import com.snowplowanalytics.iglu.server.codecs._
import com.snowplowanalytics.iglu.server.storage.Storage
import com.snowplowanalytics.iglu.server.middleware.PermissionMiddleware
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Permission, Schema, VersionCursor}
import com.snowplowanalytics.iglu.server.model.Schema.{SchemaBody, SupersedingInfo}
import com.snowplowanalytics.iglu.server.model.Schema.Repr.{Format => SchemaFormat}
import com.snowplowanalytics.iglu.server.model.VersionCursor.Inconsistency

class SchemaService[F[+_]: Sync](
  swagger: SwaggerSyntax[F],
  ctx: AuthedContext[F, Permission],
  db: Storage[F],
  patchesAllowed: Boolean,
  webhooks: Webhook.WebhookClient[F]
) extends RhoRoutes[F] {

  import swagger._
  import SchemaService._
  implicit val C: Clock[F] = Clock.create[F]

  val reprUri       = genericQueryCapture(UriParsers.parseRepresentationUri[F]).withMetadata(ReprMetadata)
  val reprCanonical = genericQueryCapture(UriParsers.parseRepresentationCanonical[F]).withMetadata(ReprMetadata)

  val version  = pathVar[SchemaVer.Full]("version", "SchemaVer")
  val isPublic = paramD[Boolean]("isPublic", false, "Should schema be created as public")

  val schemaOrJson = jsonOf[F, SchemaBody]

  private val validationService = new ValidationService[F](swagger, ctx, db)

  "Get a particular schema by its Iglu URI" **
    GET / 'vendor / 'name / 'format / version +? reprCanonical >>> ctx.auth |>> getSchema _

  "Delete a particular schema from Iglu by its URI" **
    DELETE / 'vendor / 'name / 'format / version >>> ctx.auth |>> deleteSchema _

  "Get list of schemas by vendor name" **
    GET / 'vendor / 'name +? reprUri >>> ctx.auth |>> getSchemasByName _

  "Get list of schemas by vendor name" **
    GET / 'vendor / 'name / "jsonschema" / pathVar[Int]("model") +? reprUri >>> ctx.auth |>> getSchemasByModel _

  "Get all schemas for vendor" **
    GET / 'vendor +? reprUri >>> ctx.auth |>> getSchemasByVendor _

  "List all available schemas" **
    GET +? reprUri >>> ctx.auth |>> listSchemas _

  "Add a schema (self-describing or not) to its Iglu URI" **
    PUT / 'vendor / 'name / 'format / version +? isPublic >>> ctx.auth ^ schemaOrJson |>> putSchema _

  "Publish new self-describing schema" **
    POST +? isPublic >>> ctx.auth ^ schemaOrJson |>> postSchema _

  "Schema validation endpoint (deprecated)" **
    POST / "validate" / 'vendor / 'name / "jsonschema" / 'version ^ jsonDecoder[F] |>> {
    (_: String, _: String, _: String, json: Json) => validationService.validateSchema(Schema.Format.Jsonschema, json)
  }

  def getSchema(
    vendor: String,
    name: String,
    format: String,
    version: SchemaVer.Full,
    schemaFormat: SchemaFormat,
    permission: Permission
  ) =
    db.getSchema(SchemaMap(vendor, name, format, version)).flatMap {
      case Some(schema) if schema.metadata.isPublic                              => Ok(schema.withFormat(schemaFormat))
      case Some(schema) if permission.canRead(schema.schemaMap.schemaKey.vendor) => Ok(schema.withFormat(schemaFormat))
      case _                                                                     => NotFound(IgluResponse.SchemaNotFound: IgluResponse)
    }

  def deleteSchema(vendor: String, name: String, format: String, version: SchemaVer.Full, permission: Permission) =
    permission match {
      case Permission.Super if patchesAllowed =>
        db.deleteSchema(SchemaMap(vendor, name, format, version)) *> Ok(
          IgluResponse.Message("Schema deleted"): IgluResponse
        )
      case Permission.Super =>
        MethodNotAllowed(IgluResponse.Message("DELETE is forbidden on production registry"): IgluResponse)
      case _ => Unauthorized(IgluResponse.Message("Not enough permissions"): IgluResponse)
    }

  def getSchemasByName(vendor: String, name: String, format: SchemaFormat, permission: Permission) = {
    val query = db.getSchemasByName(vendor, name).filter(isReadable(permission)).map(_.withFormat(format))
    schemasOrNotFound(query)
  }

  def getSchemasByVendor(
    vendor: String,
    format: SchemaFormat,
    permission: Permission
  ) = {
    val query = db.getSchemasByVendor(vendor).filter(isReadable(permission)).map(_.withFormat(format))
    schemasOrNotFound(query)
  }

  def getSchemasByModel(
    vendor: String,
    name: String,
    model: Int,
    format: SchemaFormat,
    permission: Permission
  ) = {
    val query = db.getSchemasByModel(vendor, name, model).filter(isReadable(permission)).map(_.withFormat(format))
    schemasOrNotFound(query)
  }

  def postSchema(isPublic: Boolean, permission: Permission, json: SchemaBody) =
    json match {
      case _: SchemaBody.BodyOnly =>
        BadRequest(IgluResponse.InvalidSchema: IgluResponse)
      case SchemaBody.SelfDescribing(schema, supersedingInfo) =>
        publishSchema(isPublic, permission, schema, supersedingInfo)
    }

  def putSchema(
    vendor: String,
    name: String,
    format: String,
    version: SchemaVer.Full,
    isPublic: Boolean,
    permission: Permission,
    json: SchemaBody
  ) = json match {
    case SchemaBody.BodyOnly(body, supersedingInfo) =>
      val schemaMap = SchemaMap(vendor, name, format, version)
      publishSchema(isPublic, permission, SelfDescribingSchema(schemaMap, body), supersedingInfo)
    case SchemaBody.SelfDescribing(schema, supersedingInfo) =>
      val schemaMapUri = SchemaMap(vendor, name, format, version)
      if (schemaMapUri == schema.self) publishSchema(isPublic, permission, schema, supersedingInfo)
      else BadRequest(IgluResponse.SchemaMismatch(schemaMapUri.schemaKey, schema.self.schemaKey): IgluResponse)
  }

  def publishSchema(
    isPublic: Boolean,
    permission: Permission,
    schema: SelfDescribingSchema[Json],
    supersedingInfo: Option[SupersedingInfo]
  ) =
    if (permission.canCreateSchema(schema.self.schemaKey.vendor)) {
      ValidationService.validateJsonSchema(schema.normalize) match {
        case Validated.Invalid(report) if report.exists(_.level == Linter.Level.Error) =>
          BadRequest(IgluResponse.SchemaValidationReport(report): IgluResponse)
        case _ => addSchema(schema, isPublic, supersedingInfo)
      }
    } else Forbidden(IgluResponse.Forbidden: IgluResponse)

  def listSchemas(format: SchemaFormat, permission: Permission) = {
    val result = format match {
      case SchemaFormat.Uri =>
        db.getSchemasKeyOnly
          .map(_.filter(isReadablePair(permission)).map {
            case (map, meta) => Schema(map, meta, Json.Null, None).withFormat(SchemaFormat.Uri)
          })
      case _ =>
        db.getSchemas.map(_.filter(isReadable(permission)).map(_.withFormat(format)))
    }
    result.flatMap(response => Ok(response))
  }

  /** Make sure that SchemaList can be only non-empty list */
  private def schemasOrNotFound(queryResult: fs2.Stream[F, Schema.Repr]) =
    queryResult.compile.toList.flatMap {
      case Nil     => NotFound(IgluResponse.Message(s"No schemas available"): IgluResponse)
      case schemas => Ok(schemas)
    }

  private def addSchema(
    schema: SelfDescribingSchema[Json],
    isPublic: Boolean,
    supersedingInfo: Option[SupersedingInfo]
  ) =
    for {
      allowed          <- isSchemaAllowed(db, schema.self, patchesAllowed, isPublic)
      supersedingCheck <- checkSupersedingVersion(schema.self, supersedingInfo)
      response <- (allowed, supersedingCheck) match {
        case (Right(_), Right(s)) =>
          for {
            existing <- db.getSchema(schema.self).map(_.isDefined)
            _ <- if (existing) db.updateSchema(schema.self, schema.schema, isPublic)
            else db.addSchema(schema.self, schema.schema, isPublic)
            _ <- updateSupersedingVersion(schema.self, s)
            payload = IgluResponse.SchemaUploaded(existing, schema.self.schemaKey): IgluResponse
            _        <- webhooks.schemaPublished(schema.self.schemaKey, existing)
            response <- if (existing) Ok(payload) else Created(payload)
          } yield response
        case (Left(Inconsistency.AlreadyExists), Right(Some(s))) =>
          for {
            _        <- updateSupersedingVersion(schema.self, Some(s))
            response <- Ok(IgluResponse.SupersedingVersionUpdated(schema.self.schemaKey): IgluResponse)
          } yield response
        case (_, Left(error)) =>
          Conflict(IgluResponse.Message(error): IgluResponse)
        case (Left(error), _) =>
          Conflict(IgluResponse.Message(error.show): IgluResponse)
      }
    } yield response

  private def updateSupersedingVersion(
    currSchema: SchemaMap,
    supersedingInfo: Option[SupersedingInfo.Pair]
  ): F[Unit] =
    supersedingInfo match {
      case None => Sync[F].unit
      case Some(value) =>
        val s = currSchema.schemaKey
        db.updateSupersedingVersion(s.vendor, s.name, value)
    }

  private def checkSupersedingVersion(
    currSchema: SchemaMap,
    supersedingInfo: Option[SupersedingInfo]
  ): F[Either[String, Option[SupersedingInfo.Pair]]] =
    supersedingInfo match {
      case Some(SupersedingInfo.Supersedes(supersededVersions)) =>
        // A schema version can only supersede a lower version of the same schema.
        val validOrdering = supersededVersions.forall { version =>
          Ordering[SchemaVer.Full].gt(currSchema.schemaKey.version, version)
        }
        EitherT
          .cond[F](
            validOrdering,
            right = SupersedingInfo.Pair(currSchema.schemaKey.version, supersededVersions).some,
            left = "There are superseded schema versions greater than superseding schema version"
          )
          .value
      case _ =>
        // An incoming schema can have the `supersededBy` property set if it was copied from another Iglu Server.
        // However, we ignore it for the purpose of updating the schema registry.
        // In other words, `supersededBy` is only used for output, but not for input.
        // This is because:
        // 1) The schema version referred by `supersededBy` must already exit, but it is also newer
        //    than the current schema version. Therefore, the current schema version already exists.
        //    This means we would need to be patching it, which is not recommended.
        // 2) When copying schemas from one Iglu Server to another (e.g. promoting schemas to PROD)
        //    in version order, `supersededBy` will refer to a future schema which would not exist yet.
        //    Therefore, we use `supersedes` instead to propagate this information.
        Sync[F].delay(Right(None))
    }
}

object SchemaService {

  val ReprMetadata: TextMetaData = new TextMetaData {
    def msg: String =
      "Schema representation format (can be specified either by repr=uri/meta/canonical or legacy meta=1&body=1)"
  }

  def asRoutes(
    patchesAllowed: Boolean,
    webhook: Webhook.WebhookClient[IO]
  )(
    db: Storage[IO],
    superKey: Option[UUID],
    ctx: AuthedContext[IO, Permission],
    rhoMiddleware: RhoMiddleware[IO]
  ): HttpRoutes[IO] = {
    val service = new SchemaService(swaggerSyntax, ctx, db, patchesAllowed, webhook).toRoutes(rhoMiddleware)
    PermissionMiddleware.wrapService(db, superKey, ctx, service)
  }

  def isSchemaAllowed[F[_]: Sync](
    db: Storage[F],
    schemaMap: SchemaMap,
    patchesAllowed: Boolean,
    isPublic: Boolean
  ): F[Either[Inconsistency, Unit]] =
    for {
      schemas <- db.getSchemasByName(schemaMap.schemaKey.vendor, schemaMap.schemaKey.name).compile.toList
      previousPublic = schemas.forall(_.metadata.isPublic)
      versions       = schemas.map(_.schemaMap.schemaKey.version)
    } yield
      if ((previousPublic && isPublic) || (!previousPublic && !isPublic) || schemas.isEmpty)
        VersionCursor.isAllowed(schemaMap.schemaKey.version, versions, patchesAllowed)
      else Inconsistency.Availability(isPublic, previousPublic).asLeft

  /** Extract schemas from database, available for particular permission */
  def isReadable(permission: Permission)(schema: Schema): Boolean =
    isReadablePair(permission)((schema.schemaMap, schema.metadata))

  def isReadablePair(permission: Permission)(meta: (SchemaMap, Schema.Metadata)): Boolean =
    meta match {
      case (schemaMap, metadata) =>
        permission.canRead(schemaMap.schemaKey.vendor) || metadata.isPublic
    }
}
