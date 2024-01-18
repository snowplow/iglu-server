/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.0
 * located at https://docs.snowplow.io/limited-use-license-1.0
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.model

import java.util.UUID

import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.show._

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

import com.snowplowanalytics.iglu.core.SchemaKey
import com.snowplowanalytics.iglu.core.circe.CirceIgluCodecs._
import com.snowplowanalytics.iglu.client.validator.ValidatorReport
import com.snowplowanalytics.iglu.schemaddl.jsonschema.Linter.{Message => LinterMessage}

trait IgluResponse extends Product with Serializable {
  def asJson: Json = Encoder[IgluResponse].apply(this)
}

object IgluResponse {

  val NotFoundSchema             = "The schema is not found"
  val NotAuthorized              = "Authentication error: not authorized"
  val Mismatch                   = "Mismatch: the schema metadata does not match the payload and URI"
  val DecodeError                = "Cannot decode JSON schema"
  val SchemaInvalidationMessage  = "The schema does not conform to a JSON Schema v4 specification"
  val DataInvalidationMessage    = "The data for a field instance is invalid against its schema"
  val NotFoundEndpoint           = "The endpoint does not exist"
  val NonSequentialSchemaVersion = "The schema version is not sequential"

  case object SchemaNotFound                                                      extends IgluResponse
  case object SchemaNonSequential                                                 extends IgluResponse
  case object EndpointNotFound                                                    extends IgluResponse
  case object InvalidSchema                                                       extends IgluResponse
  case class SchemaMismatch(uriSchemaKey: SchemaKey, payloadSchemaKey: SchemaKey) extends IgluResponse
  case class SchemaUploaded(updated: Boolean, location: SchemaKey)                extends IgluResponse

  /** Generic human-readable message, used everywhere as a fallback */
  case class Message(message: String) extends IgluResponse

  case class ApiKeys(read: UUID, write: UUID) extends IgluResponse

  case object Forbidden extends IgluResponse

  case class SchemaValidationReport(report: NonEmptyList[LinterMessage])     extends IgluResponse
  case class InstanceValidationReport(report: NonEmptyList[ValidatorReport]) extends IgluResponse

  case class SupersedingVersionUpdated(schemeKey: SchemaKey) extends IgluResponse

  implicit val responsesEncoder: Encoder[IgluResponse] =
    Encoder.instance {
      case SchemaNotFound =>
        Json.fromFields(List("message" -> Json.fromString(NotFoundSchema)))
      case EndpointNotFound =>
        Json.fromFields(List("message" -> Json.fromString(NotFoundEndpoint)))
      case Message(message) =>
        Json.fromFields(List("message" -> Json.fromString(message)))
      case Forbidden =>
        Json.fromFields(List("message" -> Json.fromString(NotAuthorized)))
      case ApiKeys(read, write) =>
        Json.fromFields(
          List(
            "read"  -> Json.fromString(read.toString),
            "write" -> Json.fromString(write.toString)
          )
        )
      case SchemaMismatch(uri, payload) =>
        Json.fromFields(
          List(
            "uriSchemaKey"     -> Json.fromString(uri.toSchemaUri),
            "payloadSchemaKey" -> Json.fromString(payload.toSchemaUri),
            "message"          -> Json.fromString(Mismatch)
          )
        )
      case SchemaUploaded(updated, location) =>
        Json.fromFields(
          List(
            "message"  -> Json.fromString(if (updated) "Schema updated" else "Schema created"),
            "updated"  -> Json.fromBoolean(updated),
            "location" -> location.toSchemaUri.asJson,
            "status"   -> Json.fromInt(if (updated) 200 else 201) // TODO: remove after igluctl 0.7.0 released
          )
        )
      case InvalidSchema =>
        Json.fromFields(List("message" -> Json.fromString(DecodeError)))
      case SchemaNonSequential =>
        Json.fromFields(List("message" -> Json.fromString(NonSequentialSchemaVersion)))
      case SchemaValidationReport(report) =>
        Json.fromFields(
          List(
            "message" -> SchemaInvalidationMessage.asJson,
            "report" -> Json.fromValues(report.toList.map { message =>
              Json.fromFields(
                List(
                  "message" -> message.message.asJson,
                  "level"   -> message.level.toString.toUpperCase.asJson,
                  "pointer" -> message.jsonPointer.show.asJson
                )
              )
            })
          )
        )
      case InstanceValidationReport(report) =>
        Json.fromFields(
          List(
            "message" -> DataInvalidationMessage.asJson,
            "report"  -> report.asJson
          )
        )
      case SupersedingVersionUpdated(schemaKey) =>
        Json.fromFields(
          List(
            "message"   -> "Superseding schema version of existing schema is updated".asJson,
            "schemaKey" -> schemaKey.toSchemaUri.asJson
          )
        )
    }

  implicit val responsesDecoder: Decoder[IgluResponse] = Decoder.instance { cur =>
    cur.downField("message").as[String] match {
      case Right(NotFoundSchema)             => SchemaNotFound.asRight
      case Right(NonSequentialSchemaVersion) => SchemaNonSequential.asRight
      case Right(NotAuthorized)              => Forbidden.asRight
      case Right(Mismatch) =>
        for {
          uriSchemaKey     <- cur.downField("uriSchemaKey").as[SchemaKey]
          payloadSchemaKey <- cur.downField("payloadSchemaKey").as[SchemaKey]
        } yield SchemaMismatch(uriSchemaKey, payloadSchemaKey)
      case Right(DecodeError) => Message(DecodeError).asRight
      case Right(message)     => Message(message).asRight
      case Left(e)            => Message(e.show).asRight
    }
  }
}
