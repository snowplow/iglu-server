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

import java.time.Instant
import io.circe._
import io.circe.syntax._
import io.circe.literal._
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer, SelfDescribingSchema}
import com.snowplowanalytics.iglu.server.model.Schema.{Metadata, Repr, SupersedingInfo}
import SchemaSpec.testSchema

class SchemaSpec extends org.specs2.Specification {
  def is = s2"""
  Decode Schema $e1
  Encode Schema $e2
  Decode SchemaBody $e3
  Encode Repr.Full $e4
  Encode Repr.Canonical $e5
  """

  def e1 = {
    val input = json"""
    {
      "self": {
        "vendor": "me.chuwy",
        "name": "test-schema",
        "format": "jsonschema",
        "version": "1-0-0"
      },
      "metadata": {
        "createdAt": "2019-01-12T22:12:54.777Z",
        "updatedAt": "2019-01-12T22:12:54.777Z",
        "isPublic": true
      },
      "type": "object"
    }"""

    val expected =
      Schema(
        SchemaMap("me.chuwy", "test-schema", "jsonschema", SchemaVer.Full(1, 0, 0)),
        Metadata(Instant.parse("2019-01-12T22:12:54.777Z"), Instant.parse("2019-01-12T22:12:54.777Z"), true),
        json"""{"type": "object"}""",
        SupersedingInfo.empty
      )

    Schema.serverSchemaDecoder.decodeJson(input) must beRight(expected)
  }

  def e2 = {
    val schemaWithoutSupersededBy =
      Schema(
        SchemaMap("me.chuwy", "test-schema", "jsonschema", SchemaVer.Full(1, 0, 0)),
        Metadata(Instant.parse("2019-01-12T22:12:54.777Z"), Instant.parse("2019-01-12T22:12:54.777Z"), true),
        json"""{"type": "object"}""",
        SupersedingInfo.empty
      )
    val schemaWithSupersededBy =
      schemaWithoutSupersededBy.copy(supersedingInfo = SupersedingInfo(Some(SchemaVer.Full(1, 0, 1)), List.empty))

    val match1 = Schema.schemaEncoder(schemaWithSupersededBy) must beEqualTo(
      json"""
        {
          "$$supersededBy": "1-0-1",
          "self": {
            "vendor": "me.chuwy",
            "name": "test-schema",
            "format": "jsonschema",
            "version": "1-0-0"
          },
          "metadata": {
            "createdAt": "2019-01-12T22:12:54.777Z",
            "updatedAt": "2019-01-12T22:12:54.777Z",
            "isPublic": true
          },
          "type": "object"
        }"""
    )

    val match2 = Schema.schemaEncoder(schemaWithoutSupersededBy) must beEqualTo(
      json"""
      {
        "self": {
          "vendor": "me.chuwy",
          "name": "test-schema",
          "format": "jsonschema",
          "version": "1-0-0"
        },
        "metadata": {
          "createdAt": "2019-01-12T22:12:54.777Z",
          "updatedAt": "2019-01-12T22:12:54.777Z",
          "isPublic": true
        },
        "type": "object"
      }"""
    )

    match1.and(match2)
  }

  def e3 = {
    val superseding = Schema.SupersedingInfo(Some(SchemaVer.Full(1, 0, 1)), List.empty)
    val superseded =
      Schema.SupersedingInfo(None, List(SchemaVer.Full(1, 0, 1), SchemaVer.Full(1, 0, 2)))

    val (schema1, schemaKey1) = testSchema(SchemaVer.Full(1, 0, 0))
    val (schema2, schemaKey2) = testSchema(
      SchemaVer.Full(1, 0, 0),
      supersedingInfo = superseding
    )
    val (schema3, schemaKey3) = testSchema(
      SchemaVer.Full(1, 0, 0),
      supersedingInfo = superseded
    )

    val bodyOnlyInput = json"""{ "type": "object" }"""
    val invalidInput  = json"""[{ "type": "object" }]"""

    val selfDescribingResult =
      Schema.SchemaBody.schemaBodyCirceDecoder.decodeJson(schema1) must beRight.like {
        case Schema
              .SchemaBody
              .SelfDescribing(
              SelfDescribingSchema(SchemaMap(`schemaKey1`), `bodyOnlyInput`),
              SupersedingInfo(None, Nil)
              ) =>
          ok
        case e => ko(s"Unexpected decoded value $e")
      }
    val supersedingSelfDescribingResult =
      Schema.SchemaBody.schemaBodyCirceDecoder.decodeJson(schema2) must beRight.like {
        case Schema
              .SchemaBody
              .SelfDescribing(SelfDescribingSchema(SchemaMap(`schemaKey2`), `bodyOnlyInput`), `superseding`) =>
          ok
        case e => ko(s"Unexpected decoded value $e")
      }
    val supersededSelfDescribingResult =
      Schema.SchemaBody.schemaBodyCirceDecoder.decodeJson(schema3) must beRight.like {
        case Schema
              .SchemaBody
              .SelfDescribing(SelfDescribingSchema(SchemaMap(`schemaKey3`), `bodyOnlyInput`), `superseded`) =>
          ok
        case e => ko(s"Unexpected decoded value $e")
      }
    val bodyOnlyResult = Schema.SchemaBody.schemaBodyCirceDecoder.decodeJson(bodyOnlyInput) must beRight.like {
      case _: Schema.SchemaBody.BodyOnly => ok
      case e                             => ko(s"Unexpected decoded value $e")
    }
    val invalidBodyResult = Schema.SchemaBody.schemaBodyCirceDecoder.decodeJson(invalidInput) must beLeft

    selfDescribingResult
      .and(supersedingSelfDescribingResult)
      .and(supersededSelfDescribingResult)
      .and(bodyOnlyResult)
      .and(invalidBodyResult)
  }

  def e4 = {
    val schema = Schema(
      SchemaMap("me.chuwy", "test-schema", "jsonschema", SchemaVer.Full(1, 0, 0)),
      Metadata(Instant.parse("2019-01-12T22:12:54.777Z"), Instant.parse("2019-01-12T22:12:54.777Z"), true),
      json"""{"type": "object"}""",
      SupersedingInfo(Some(SchemaVer.Full(1, 0, 1)), List.empty)
    )
    val reprWithSupersededBy    = Repr.Full(schema)
    val reprWithoutSupersededBy = Repr.Full(schema.copy(supersedingInfo = SupersedingInfo.empty))

    val match1 = Schema.representationEncoder(reprWithSupersededBy).noSpaces must beEqualTo(
      json"""
      {
        "$$supersededBy": "1-0-1",
        "self": {
          "vendor": "me.chuwy",
          "name": "test-schema",
          "format": "jsonschema",
          "version": "1-0-0"
        },
        "metadata": {
          "createdAt": "2019-01-12T22:12:54.777Z",
          "updatedAt": "2019-01-12T22:12:54.777Z",
          "isPublic": true
        },
        "type": "object"
      }""".noSpaces
    )
    val match2 = Schema.representationEncoder(reprWithoutSupersededBy).noSpaces must beEqualTo(
      json"""
      {
        "self": {
          "vendor": "me.chuwy",
          "name": "test-schema",
          "format": "jsonschema",
          "version": "1-0-0"
        },
        "metadata": {
          "createdAt": "2019-01-12T22:12:54.777Z",
          "updatedAt": "2019-01-12T22:12:54.777Z",
          "isPublic": true
        },
        "type": "object"
      }""".noSpaces
    )
    match1.and(match2)
  }

  def e5 = {
    val schema = SelfDescribingSchema(
      SchemaMap("me.chuwy", "test-schema", "jsonschema", SchemaVer.Full(1, 0, 0)),
      json"""{"type": "object"}"""
    )
    val reprWithSupersededBy = Repr.Canonical(
      schema,
      SupersedingInfo(Some(SchemaVer.Full(1, 0, 1)), List.empty)
    )
    val reprWithoutSupersededBy = Repr.Canonical(schema, SupersedingInfo.empty)

    val match1 = Schema.representationEncoder(reprWithSupersededBy).noSpaces must beEqualTo(
      json"""
      {
        "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
        "$$supersededBy": "1-0-1",
        "self": {
          "vendor": "me.chuwy",
          "name": "test-schema",
          "format": "jsonschema",
          "version": "1-0-0"
        },
        "type": "object"
      }""".noSpaces
    )
    val match2 = Schema.representationEncoder(reprWithoutSupersededBy).noSpaces must beEqualTo(
      json"""
      {
        "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
        "self": {
          "vendor": "me.chuwy",
          "name": "test-schema",
          "format": "jsonschema",
          "version": "1-0-0"
        },
        "type": "object"
      }""".noSpaces
    )
    match1.and(match2)
  }
}

object SchemaSpec {
  def testSchema(
    version: SchemaVer.Full,
    supersedingInfo: Schema.SupersedingInfo = Schema.SupersedingInfo.empty,
    mergeJson: Json = JsonObject.empty.asJson
  ): (Json, SchemaKey) = {
    val schemaKey = SchemaKey("com.acme", "nonexistent", "jsonschema", version)
    val supersedingInfoJson =
      json"""
      {
        "$$supersededBy": ${supersedingInfo.supersededBy.map(_.asString)},
        "$$supersedes": ${supersedingInfo.supersedes.map(_.asString).asJson}
      }""".dropNullValues.dropEmptyValues
    val schema =
      json"""
      {
        "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
        "self": {
          "vendor": ${schemaKey.vendor},
          "name": ${schemaKey.name},
          "format": ${schemaKey.format},
          "version": ${version.asString}
        },
        "type": "object"
      }"""
    (schema.deepMerge(supersedingInfoJson).deepMerge(mergeJson), schemaKey)
  }
}
