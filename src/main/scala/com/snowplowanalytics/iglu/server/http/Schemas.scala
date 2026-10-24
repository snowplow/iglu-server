/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server
package http

import sttp.tapir.{FieldName, Schema, SchemaType, Validator}
import sttp.tapir.Schema.SName
import sttp.tapir.SchemaType.{SArray, SProduct, SProductField}
import sttp.tapir.json.circe.schemaForCirceJson

import com.snowplowanalytics.iglu.core.SchemaMap

import com.snowplowanalytics.iglu.server.model.{
  DraftVersion,
  IgluResponse,
  Permission,
  SchemaDraft,
  Schema => IgluSchema
}
import com.snowplowanalytics.iglu.server.service.AuthService.GenerateKey
import com.snowplowanalytics.iglu.server.service.MetaService.ServerInfo

/** Schemas of the request and response bodies, for the OpenAPI documentation */
object Schemas {

  /**
    * The schema of `s`, for a value of another type, e.g. a JSON body read as a string. The default and the
    * validator of `s` are dropped, as they are of its own type, and tapir would validate the decoded `U` with them.
    */
  def as[U](s: Schema[_]): Schema[U] =
    s.as[U].copy(default = None, validator = Validator.pass)

  // Hand-written: the circe codecs of these types are custom, so derived schemas would not describe their JSON

  private def obj[T](name: String, description: String, fields: (String, Schema[_])*): Schema[T] =
    Schema[T](
      SProduct(fields.toList.map { case (n, s) => SProductField[T, Any](FieldName(n), s.as[Any], _ => None) }),
      name = Some(SName(name)),
      description = Some(description)
    )

  private def string[T](description: String): Schema[T] =
    Schema[T](SchemaType.SString[T](), description = Some(description))

  implicit val igluResponseSchema: Schema[IgluResponse] =
    obj[IgluResponse](
      "IgluResponse",
      "Iglu Server generic response",
      "message" -> Schema
        .schemaForString
        .description(
          "Human-readable message. The only required property for all kinds of responses"
        )
    )

  implicit val schemaReprSchema: Schema[IgluSchema.Repr] =
    obj[IgluSchema.Repr](
      "Repr",
      "Schema in the requested representation: canonical self-describing JSON Schema, Iglu URI, or with metadata",
      "self" -> obj[SchemaMap](
        "SchemaMap",
        "Iglu URI of the schema",
        "vendor"  -> Schema.schemaForString,
        "name"    -> Schema.schemaForString,
        "format"  -> Schema.schemaForString,
        "version" -> Schema.schemaForString
      )
    )

  implicit val schemaBodySchema: Schema[IgluSchema.SchemaBody] =
    obj[IgluSchema.SchemaBody]("SchemaBody", "JSON Schema, self-describing or not")

  implicit val vendorSchema: Schema[Permission.Vendor] =
    string[Permission.Vendor]("Vendor prefix")

  implicit val schemaActionSchema: Schema[Permission.SchemaAction] =
    string[Permission.SchemaAction]("Permission on schemas: READ, BUMP, CREATE or CREATE_VENDOR")

  implicit val keyActionSchema: Schema[Permission.KeyAction] =
    string[Permission.KeyAction]("Permission on API keys: CREATE or DELETE")

  implicit val draftVersionSchema: Schema[DraftVersion] =
    Schema[DraftVersion](SchemaType.SInteger[DraftVersion]())

  // Derived: the circe codecs of these types are derived from the case classes too

  implicit val permissionSchema: Schema[Permission]        = Schema.derived[Permission]
  implicit val serverInfoSchema: Schema[ServerInfo]        = Schema.derived[ServerInfo]
  implicit val metadataSchema: Schema[IgluSchema.Metadata] = Schema.derived[IgluSchema.Metadata]
  implicit val draftIdSchema: Schema[SchemaDraft.DraftId]  = Schema.derived[SchemaDraft.DraftId]
  implicit val schemaDraftSchema: Schema[SchemaDraft]      = Schema.derived[SchemaDraft]
  implicit val keyPairSchema: Schema[Permission.KeyPair]   = Schema.derived[Permission.KeyPair]
  implicit val generateKeySchema: Schema[GenerateKey]      = Schema.derived[GenerateKey]

  /** The drafts as listed by `DraftService`: concatenated objects, which no JSON schema describes */
  val schemaDrafts: Schema[String] =
    Schema(SArray[String, SchemaDraft](schemaDraftSchema)(_ => Nil), description = Some("Schema drafts, concatenated"))
}
