/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.service

import io.circe.Json

import sttp.model.StatusCode
import sttp.tapir.{PublicEndpoint, path, stringToPath}
import sttp.tapir.json.circe.schemaForCirceJson

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.Applicative
import cats.effect.Async
import cats.implicits._

import com.snowplowanalytics.iglu.core.{SchemaMap, SelfDescribingData, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.implicits._

import com.snowplowanalytics.iglu.client.validator.ValidatorError
import com.snowplowanalytics.iglu.client.validator.CirceValidator
import com.snowplowanalytics.iglu.schemaddl.jsonschema.circe.implicits._
import com.snowplowanalytics.iglu.schemaddl.jsonschema.{Pointer, SelfSyntaxChecker, Schema => SchemaAst}
import com.snowplowanalytics.iglu.schemaddl.jsonschema.SanityLinter.lint
import com.snowplowanalytics.iglu.schemaddl.jsonschema.Linter.{Level, Message, allLintersMap}

import com.snowplowanalytics.iglu.server.Utils
import com.snowplowanalytics.iglu.server.http.Api
import com.snowplowanalytics.iglu.server.http.Endpoints._
import com.snowplowanalytics.iglu.server.http.Schemas.igluResponseSchema
import com.snowplowanalytics.iglu.server.storage.Storage
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Permission, Schema}

object ValidationService {

  def validateSchemaEndpoint: PublicEndpoint[(Schema.Format, String), Failure, IgluResponse, Any] =
    base(StatusCode.Forbidden)
      .post
      .in("validate" / "schema" / path[Schema.Format]("format").description("Schema format, e.g. jsonschema"))
      .in(jsonIn[Json])
      .out(jsonOut[IgluResponse])
      .summary("This route allows you to validate schemas")

  def validateInstanceEndpoint: PublicEndpoint[String, Failure, IgluResponse, Any] =
    base(StatusCode.BadRequest, StatusCode.NotFound, StatusCode.InternalServerError)
      .post
      .in("validate" / "instance")
      .in(jsonIn[Json])
      .out(jsonOut[IgluResponse])
      .summary("This route allows you to validate self-describing instances")

  def api[F[_]: Async](maxJsonDepth: Int)(db: Storage[F]): Api[F] = {
    val jsonBody = Utils.jsonDecoderWithDepthCheck[F](maxJsonDepth)

    Api(
      List(
        Api.withPermission(validateSchemaEndpoint) { permission =>
          {
            case (format, body) =>
              withBody(body, jsonBody)(json => Async[F].pure(validateSchema(format, permission, json, maxJsonDepth)))
          }
        },
        Api.withPermission(validateInstanceEndpoint) { permission => body =>
          withBody(body, jsonBody)(validateData(db, permission, _))
        }
      )
    )
  }

  def validateSchema(
    format: Schema.Format,
    authInfo: Permission,
    schema: Json,
    maxJsonDepth: Int
  ): Either[Failure, IgluResponse] =
    if (!authInfo.canValidate)
      Failure.text(StatusCode.Forbidden, "").asLeft
    else
      format match {
        case Schema.Format.Jsonschema =>
          validateJsonSchema(schema, maxJsonDepth) match {
            case Validated.Valid(sd) =>
              val message = s"The schema provided is a valid self-describing ${sd.self.schemaKey.toSchemaUri} schema"
              (IgluResponse.Message(message): IgluResponse).asRight
            case Validated.Invalid(report) =>
              (IgluResponse.SchemaValidationReport(report): IgluResponse).asRight
          }
      }

  def validateData[F[_]: Applicative](
    db: Storage[F],
    authInfo: Permission,
    instance: Json
  ): F[Either[Failure, IgluResponse]] =
    if (!authInfo.canValidate)
      Applicative[F].pure(Failure.json(StatusCode.NotFound, IgluResponse.SchemaNotFound).asLeft)
    else
      SelfDescribingData.parse(instance) match {
        case Right(SelfDescribingData(key, data)) =>
          db.getSchema(SchemaMap(key)).map {
            case Some(Schema(_, meta, schemaBody, _)) if meta.isPublic || authInfo.canRead(key.vendor) =>
              CirceValidator.validate(data, schemaBody) match {
                case Left(ValidatorError.InvalidData(report)) =>
                  (IgluResponse.InstanceValidationReport(report): IgluResponse).asRight
                case Left(ValidatorError.InvalidSchema(_)) =>
                  val message = s"Schema ${key.toSchemaUri} fetched from DB is invalid"
                  Failure.json(StatusCode.InternalServerError, IgluResponse.Message(message)).asLeft
                case Right(_) =>
                  (IgluResponse.Message(s"Instance is valid ${key.toSchemaUri}"): IgluResponse).asRight
              }
            case _ =>
              Failure.json(StatusCode.NotFound, IgluResponse.SchemaNotFound).asLeft
          }
        case Left(error) =>
          Applicative[F].pure(
            Failure
              .json(StatusCode.BadRequest, IgluResponse.Message(s"JSON payload is not self-describing, ${error.code}"))
              .asLeft
          )
      }

  type LintReport[A] = ValidatedNel[Message, A]

  val NotSelfDescribing = Message(Pointer.Root, "JSON Schema is not self-describing", Level.Error)
  val NotSchema         = Message(Pointer.Root, "Cannot extract JSON Schema", Level.Error)

  def validateJsonSchema(schema: Json, maxJsonDepth: Int): LintReport[SelfDescribingSchema[Json]] = {
    val generalCheck =
      SelfSyntaxChecker.validateSchema(schema, maxJsonDepth)

    val selfDescribingCheck = SelfDescribingSchema
      .parse(schema)
      .fold(_ => NotSelfDescribing.invalidNel[SelfDescribingSchema[Json]], _.validNel[Message])

    val lintReport: LintReport[Unit] =
      SchemaAst.parse(schema).fold(NotSchema.invalidNel[SchemaAst])(_.validNel[Message]).andThen { ast =>
        val result = lint(ast, allLintersMap.values.toList).toList.flatMap {
          case (pointer, issues) => issues.toList.map(_.toMessage(pointer))
        }
        NonEmptyList.fromList(result).fold(().validNel[Message])(_.invalid[Unit])
      }

    (generalCheck, lintReport, selfDescribingCheck).mapN((_, _, schema) => schema)
  }
}
