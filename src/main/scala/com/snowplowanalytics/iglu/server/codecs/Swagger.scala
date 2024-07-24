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
package codecs

import scala.reflect.runtime.universe.typeOf

import cats.syntax.option._

import io.circe.Json

import org.http4s.rho.swagger.DefaultSwaggerFormats
import org.http4s.rho.swagger.models._

import com.snowplowanalytics.iglu.core.SchemaKey
import com.snowplowanalytics.iglu.server.model.IgluResponse
import com.snowplowanalytics.iglu.server.model.Schema

object Swagger {

  def Formats =
    DefaultSwaggerFormats
      .withSerializers(typeOf[IgluResponse], exampleModel)
      .withSerializers(typeOf[Schema.Repr.Canonical], exampleModel)
      .withSerializers(typeOf[JsonCodecs.JsonArrayStream[cats.effect.IO, Schema.Repr]], exampleModel)
      .withSerializers(typeOf[Json], exampleModel)
      .withSerializers(typeOf[SchemaKey], exampleModel)

  val exampleModel: Set[Model] = Set(
    ModelImpl(
      id = "IgluResponse",
      id2 = "IgluResponse", // Somehow, only id2 works
      `type` = "object".some,
      description = "Iglu Server generic response".some,
      properties = Map(
        "message" -> StringProperty(
          required = true,
          description = "Human-readable message. The only required property for all kinds of responses".some,
          enums = Set()
        )
      ),
      example = """{"message" : "Schema does not exist"}""".some
    ),
    ModelImpl(
      id = "JsonArrayStream",
      id2 = "JsonArrayStream«F,Repr»",
      name = "Array".some,
      `type` = "array".some,
      description = "Generic JSON array JSON Schema representations".some,
      properties = Map(),
      example = """[]""".some
    ),
    ModelImpl(
      id = "Canonical",
      id2 = "Canonical",
      `type` = "object".some,
      description = "Canonical representation of self-describing JSON Schema".some,
      properties = Map(
        "self" -> ObjectProperty(
          required = true,
          properties = Map("name" -> StringProperty(enums = Set()))
        )
      ),
      example = """{"self": {"name": "event", "vendor": "com.acme", "format": "jsonschema", "version": "1-0-0"}}""".some
    ),
    ModelImpl(
      id = "SchemaKey",
      id2 = "SchemaKey",
      `type` = "string".some,
      description = "Canonical iglu URI".some,
      properties = Map(),
      example = """iglu:com.snowplowanalytics/geo_location/jsonschema/1-0-0""".some
    ),
    ModelImpl(
      id = "Json",
      id2 = "Json",
      `type` = "string".some,
      description = "Any valid JSON".some,
      properties = Map(),
      example = """{"foo": null}""".some
    )
  )
}
