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

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.IO

import fs2.Stream

import io.circe._
import io.circe.literal._

import org.http4s._
import org.http4s.implicits._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.rho.swagger.syntax.io.createRhoMiddleware

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer, SelfDescribingSchema}
import com.snowplowanalytics.iglu.server.codecs.JsonCodecs._
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Schema}

import com.snowplowanalytics.iglu.server.model.SchemaSpec.testSchema

import SchemaServiceSpec._

class SchemaServiceSpec extends org.specs2.Specification {
  def is = s2"""
  GET
    Returns 404 for non-existing schema $e1
    Returns 200 and schema for existing public schema $e3
    Returns 200 and schema for existing private schema $e4
    Returns list of schemas with metadata if repr=meta passed $e9
    Returns list of canonical schemas if repr=canonical passed $e10
    Returns 404 for existing schema if invalid apikey provided $e5
    Returns only public schemas without apikey $e7
    Returns public and private schemas with apikey $e8
    Returns error for non-UUID apikey $e6
    Returns 404 and meaningful error for invalid SchemaVer $e14
    Returns schemas ordered by pub_date $e15
  PUT
    Prohibits adding new schema if it already exists $e11
    Prohibits adding new schema if previous version does not exist $e12
    PUT request adds schema $e2
    PUT request updates existing schema if patches are allowed $e13
    Prohibits adding new schema if it is not valid $e16
    PUT request with only schema body adds schema $e17
    PUT request with 'supersededBy' field adds schema with superseding info when patches aren't allowed $e18
    PUT request with 'supersedes' field adds schema with superseding info $e19
    Prohibits adding schema superseded by version smaller than itself $e20
  """

  def e1 = {
    val req: Request[IO] =
      Request(Method.GET, uri"/com.acme/nonexistent/jsonschema/1-0-0")

    val response = SchemaServiceSpec.request(List(req), false)
    response.unsafeRunSync().status must beEqualTo(Status.NotFound)
  }

  def e3 = {
    val req: Request[IO] =
      Request(Method.GET, uri"/com.acme/event/jsonschema/1-0-0")

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[Json]
    } yield (response.status, body)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.Ok)).and(body must beEqualTo(SpecHelpers.selfSchemaZero))
  }

  def e4 = {
    val req: Request[IO] =
      Request(
        Method.GET,
        Uri.uri("/com.acme/secret/jsonschema/1-0-0"),
        headers = Headers.of(Header("apikey", SpecHelpers.readKeyAcme.toString))
      )

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[Json]
    } yield (response.status, body)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.Ok)).and(body must beEqualTo(SpecHelpers.selfSchemaPrivate))
  }

  def e5 = {
    val req: Request[IO] =
      Request(
        Method.GET,
        Uri.uri("/com.acme/secret/jsonschema/1-0-0"),
        headers = Headers.of(Header("apikey", UUID.randomUUID().toString))
      )

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[IgluResponse]
    } yield (response.status, body)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.NotFound)).and(body must beEqualTo(IgluResponse.SchemaNotFound))
  }

  def e9 = {
    val req: Request[IO] =
      Request(Method.GET, Uri.uri("/").withQueryParam("repr", "meta"))

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[List[Schema]]
    } yield (response.status, body)

    val expectedBody = SpecHelpers.schemas.filter { case (_, m) => m.metadata.isPublic }.map(_._2)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.Ok)).and(body must beEqualTo(expectedBody))
  }

  def e10 = {
    val req: Request[IO] =
      Request(Method.GET, Uri.uri("/").withQueryParam("repr", "canonical"))

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[List[SelfDescribingSchema[Json]]]
    } yield (response.status, body.map(_.self))

    val expectedBody = SpecHelpers.schemas.filter { case (_, m) => m.metadata.isPublic }.map(_._2.schemaMap)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.Ok)).and(body must beEqualTo(expectedBody))
  }

  def e6 = {
    val req: Request[IO] =
      Request(
        Method.GET,
        Uri.uri("/com.acme/secret/jsonschema/1-0-0"),
        headers = Headers.of(Header("apikey", "not-uuid"))
      )

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[Json]
    } yield (response.status, body)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.BadRequest)).and(body.noSpaces must contain("Invalid UUID"))
  }

  def e7 = {
    val req: Request[IO] =
      Request(Method.GET, Uri.uri("/"))

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[List[SchemaKey]]
    } yield (response.status, body)

    val expected = List(
      SchemaKey("com.acme", "event", "jsonschema", SchemaVer.Full(1, 0, 0)),
      SchemaKey("com.acme", "event", "jsonschema", SchemaVer.Full(1, 0, 1))
    )
    val (status, body) = result.unsafeRunSync()

    (status must beEqualTo(Status.Ok)).and(body must beEqualTo(expected))
  }

  def e8 = {
    val req: Request[IO] =
      Request(Method.GET, uri"/").withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[List[SchemaKey]]
    } yield (response.status, body)

    val expected = List(
      SchemaKey("com.acme", "event", "jsonschema", SchemaVer.Full(1, 0, 0)),
      SchemaKey("com.acme", "event", "jsonschema", SchemaVer.Full(1, 0, 1)),
      SchemaKey("com.acme", "secret", "jsonschema", SchemaVer.Full(1, 0, 0))
    )
    val (status, body) = result.unsafeRunSync()

    (status must beEqualTo(Status.Ok)).and(body must beEqualTo(expected))
  }

  def e2 = {
    val selfDescribingSchema =
      json"""
        {
          "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
          "self": {
            "vendor": "com.acme",
            "name": "nonexistent",
            "format": "jsonschema",
            "version": "1-0-0"
          },
          "type": "object"
        }"""
    val exampleSchema = Stream.emits(selfDescribingSchema.noSpaces.stripMargin.getBytes).covary[IO]

    val reqs: List[Request[IO]] = List(
      Request[IO](Method.PUT, uri"/com.acme/nonexistent/jsonschema/1-0-0")
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchema),
      Request[IO](Method.GET, uri"/com.acme/nonexistent/jsonschema/1-0-0")
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
    )

    val (requests, state) = SchemaServiceSpec.state(reqs, false).unsafeRunSync()
    val dbExpectation = state.schemas.mapValues(s => (s.metadata.isPublic, s.body)) must havePair(
      (
        SchemaMap("com.acme", "nonexistent", "jsonschema", SchemaVer.Full(1, 0, 0)),
        (false, json"""{"type": "object"}""")
      )
    )
    val requestExpectation = requests.lastOption.map(_.status) must beSome(Status.Ok)
    dbExpectation.and(requestExpectation)
  }

  def e11 = {
    val selfDescribingSchema =
      json"""
        {
          "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
          "self": {
            "vendor": "com.acme",
            "name": "nonexistent",
            "format": "jsonschema",
            "version": "1-0-0"
          },
          "type": "object"
        }"""
    val exampleSchema = Stream.emits(selfDescribingSchema.noSpaces.stripMargin.getBytes).covary[IO]

    val selfDescribingSchemaUpdated =
      json"""
        {
          "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
          "self": {
            "vendor": "com.acme",
            "name": "nonexistent",
            "format": "jsonschema",
            "version": "1-0-0"
          },
          "type": "object",
          "additionalProperties": true
        }"""
    val exampleSchemaUpdated = Stream.emits(selfDescribingSchemaUpdated.noSpaces.stripMargin.getBytes).covary[IO]

    val reqs: List[Request[IO]] = List(
      Request[IO](Method.PUT, Uri.uri("/com.acme/nonexistent/jsonschema/1-0-0"))
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchema),
      Request[IO](Method.PUT, Uri.uri("/com.acme/nonexistent/jsonschema/1-0-0"))
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchemaUpdated),
      Request[IO](Method.GET, Uri.uri("/com.acme/nonexistent/jsonschema/1-0-0"))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
    )

    val (requests, state) = SchemaServiceSpec.state(reqs, false).unsafeRunSync()
    val dbExpectation = state.schemas.mapValues(s => (s.metadata.isPublic, s.body)) must havePair(
      (
        SchemaMap("com.acme", "nonexistent", "jsonschema", SchemaVer.Full(1, 0, 0)),
        (false, json"""{"type": "object"}""")
      )
    )
    val putRequestExpectation = requests.get(1).map(_.status) must beSome(Status.Conflict)
    val getRequestExpectation = requests.lastOption.map(_.status) must beSome(Status.Ok)
    dbExpectation.and(putRequestExpectation).and(getRequestExpectation)
  }

  def e12 = {
    val selfDescribingSchema =
      json"""
        {
          "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
          "self": {
            "vendor": "com.acme",
            "name": "nonexistent",
            "format": "jsonschema",
            "version": "1-2-0"
          },
          "type": "object"
        }"""
    val exampleSchema = Stream.emits(selfDescribingSchema.noSpaces.stripMargin.getBytes).covary[IO]

    val req = Request[IO](Method.PUT, Uri.uri("/com.acme/nonexistent/jsonschema/1-2-0"))
      .withContentType(headers.`Content-Type`(MediaType.application.json))
      .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
      .withBodyStream(exampleSchema)

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[Json]
    } yield (response.status, body)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.Conflict)).and(
      body.noSpaces must contain(
        "Preceding SchemaVer in the group is missing, check that schemas published in proper order"
      )
    )
  }

  def e13 = {
    val selfDescribingSchema =
      json"""
        {
          "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
          "self": {
            "vendor": "com.acme",
            "name": "nonexistent",
            "format": "jsonschema",
            "version": "1-0-0"
          },
          "type": "object"
        }"""
    val exampleSchema = Stream.emits(selfDescribingSchema.noSpaces.stripMargin.getBytes).covary[IO]

    val selfDescribingSchemaUpdated =
      json"""
        {
          "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
          "self": {
            "vendor": "com.acme",
            "name": "nonexistent",
            "format": "jsonschema",
            "version": "1-0-0"
          },
          "type": "object",
          "additionalProperties": true
        }"""
    val exampleSchemaUpdated = Stream.emits(selfDescribingSchemaUpdated.noSpaces.stripMargin.getBytes).covary[IO]

    val reqs: List[Request[IO]] = List(
      Request[IO](Method.PUT, Uri.uri("/com.acme/nonexistent/jsonschema/1-0-0"))
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchema),
      Request[IO](Method.PUT, Uri.uri("/com.acme/nonexistent/jsonschema/1-0-0"))
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchemaUpdated),
      Request[IO](Method.GET, Uri.uri("/com.acme/nonexistent/jsonschema/1-0-0"))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
    )

    val (requests, state) = SchemaServiceSpec.state(reqs, true).unsafeRunSync()
    val dbExpectation = state.schemas.mapValues(s => (s.metadata.isPublic, s.body)) must havePair(
      (
        SchemaMap("com.acme", "nonexistent", "jsonschema", SchemaVer.Full(1, 0, 0)),
        (false, json"""{"type": "object", "additionalProperties": true}""")
      )
    )
    val requestExpectation = requests.lastOption.map(_.status) must beSome(Status.Ok)
    dbExpectation.and(requestExpectation)
  }

  def e14 = {
    val req: Request[IO] =
      Request(Method.GET, uri"/com.acme/nonexistent/jsonschema/boom")

    val response = SchemaServiceSpec.request(List(req), false)

    val result = for {
      r    <- response
      body <- r.bodyText.compile.foldMonoid
    } yield (r.status, body)

    // Text body transformed to JSON later in HttpApp
    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.BadRequest))
      .and(body must beEqualTo("Cannot parse version part 'boom' as SchemaVer, INVALID_SCHEMAVER"))
  }

  def e15 = {
    val simpleSchema  = json"""{"type": "object"}"""
    val exampleSchema = Stream.emits(simpleSchema.noSpaces.stripMargin.getBytes).covary[IO]

    val reqs: List[Request[IO]] = List(
      Request[IO](Method.PUT, uri"/com.acme/nonexistent/jsonschema/1-0-0")
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchema),
      Request[IO](Method.PUT, uri"/com.acme/nonexistent/jsonschema/1-0-1")
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchema),
      Request[IO](Method.PUT, uri"/com.acme/nonexistent/jsonschema/2-0-0")
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchema),
      Request[IO](Method.PUT, uri"/com.acme/nonexistent/jsonschema/1-0-2")
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchema),
      Request[IO](Method.PUT, uri"/com.acme/nonexistent/jsonschema/1-1-0")
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchema),
      Request[IO](Method.GET, uri"/com.acme/nonexistent/jsonschema/1")
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
    )

    val expected =
      """["iglu:com.acme/nonexistent/jsonschema/1-0-0","iglu:com.acme/nonexistent/jsonschema/1-0-1","iglu:com.acme/nonexistent/jsonschema/1-0-2","iglu:com.acme/nonexistent/jsonschema/1-1-0"]"""

    val result = for {
      response <- SchemaServiceSpec.request(reqs, false)
      last     <- response.bodyText.compile.foldMonoid
    } yield last

    result.unsafeRunSync() must beEqualTo(expected)
  }

  def e16 = {
    val selfDescribingSchema =
      json"""
        {
          "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
          "self": {
            "vendor": "com.acme",
            "name": "nonexistent",
            "format": "jsonschema",
            "version": "1-0-0"
          },
          "type": "object",
          "properties": {},
          "additionalProperties": false,
          "required": ["xyz"]
        }"""
    val exampleSchema = Stream.emits(selfDescribingSchema.noSpaces.stripMargin.getBytes).covary[IO]

    val req = Request[IO](Method.PUT, Uri.uri("/com.acme/nonexistent/jsonschema/1-0-0"))
      .withContentType(headers.`Content-Type`(MediaType.application.json))
      .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
      .withBodyStream(exampleSchema)

    val result = for {
      response <- SchemaServiceSpec.request(List(req), false)
      body     <- response.as[Json]
    } yield (response.status, body)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.BadRequest)).and(
      body.noSpaces must contain(
        "Elements specified as required [xyz] don't exist in schema properties"
      )
    )
  }

  def e17 = {
    val simpleSchema =
      json"""
        {
          "type": "object",
          "properties": {
            "field_a": {
              "type": "string"
            }
          }
        }"""
    val exampleSchema = Stream.emits(simpleSchema.noSpaces.stripMargin.getBytes).covary[IO]

    val expectedSchema =
      json"""
        {
          "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
          "self": {
            "vendor": "com.acme",
            "name": "nonexistent",
            "format": "jsonschema",
            "version": "1-0-0"
          },
          "type": "object",
          "properties": {
            "field_a": {
              "type": "string"
            }
          }
        }"""

    val reqs = List(
      Request[IO](Method.PUT, Uri.uri("/com.acme/nonexistent/jsonschema/1-0-0"))
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withBodyStream(exampleSchema),
      Request[IO](Method.GET, uri"/com.acme/nonexistent/jsonschema/1-0-0")
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
    )

    val result = for {
      response <- SchemaServiceSpec.request(reqs, false)
      body     <- response.as[Json]
    } yield (response.status, body)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.Ok)).and(
      body must beEqualTo(expectedSchema)
    )
  }

  def e18 = {
    val (schema100, schemaKey100) = testSchema(SchemaVer.Full(1, 0, 0))
    val (schema101, schemaKey101) = testSchema(SchemaVer.Full(1, 0, 1))
    val (schema100SupersededBy, _) = testSchema(
      version = SchemaVer.Full(1, 0, 0),
      supersedingInfo = Schema.SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 1)).some,
      json"""{
        "properties": {
          "field_a": {
            "type": "string"
          }
        }
      }"""
    )
    val (expectedSchema100, _) = testSchema(
      version = SchemaVer.Full(1, 0, 0),
      supersedingInfo = Schema.SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 1)).some
    )

    val reqs = List(
      Request[IO](Method.PUT, schemaKey100.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema100),
      Request[IO](Method.PUT, schemaKey101.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema101),
      Request[IO](Method.PUT, schemaKey100.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema100SupersededBy),
      Request[IO](Method.GET, schemaKey100.uri).withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
    )

    val result = for {
      response <- SchemaServiceSpec.request(reqs, false)
      body     <- response.as[Json]
    } yield (response.status, body)

    val (status, body) = result.unsafeRunSync()
    (status must beEqualTo(Status.Ok)).and(
      body must beEqualTo(expectedSchema100)
    )
  }

  def e19 = {
    val (schema100, schemaKey100) = testSchema(SchemaVer.Full(1, 0, 0))
    val (schema101, schemaKey101) = testSchema(SchemaVer.Full(1, 0, 1))
    val (schema102, schemaKey102) = testSchema(SchemaVer.Full(1, 0, 2))
    val (schema103, schemaKey103) = testSchema(
      version = SchemaVer.Full(1, 0, 3),
      supersedingInfo = Schema
        .SupersedingInfo
        .Superseded(
          NonEmptyList.of(
            SchemaVer.Full(1, 0, 0),
            SchemaVer.Full(1, 0, 1),
            SchemaVer.Full(1, 0, 2)
          )
        )
        .some
    )

    val (expectedSchema100, _) = testSchema(
      version = SchemaVer.Full(1, 0, 0),
      supersedingInfo = Schema.SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 3)).some
    )
    val (expectedSchema101, _) = testSchema(
      version = SchemaVer.Full(1, 0, 1),
      supersedingInfo = Schema.SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 3)).some
    )
    val (expectedSchema102, _) = testSchema(
      version = SchemaVer.Full(1, 0, 2),
      supersedingInfo = Schema.SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 3)).some
    )

    val reqs = List(
      Request[IO](Method.PUT, schemaKey100.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema100),
      Request[IO](Method.PUT, schemaKey101.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema101),
      Request[IO](Method.PUT, schemaKey102.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema102),
      Request[IO](Method.PUT, schemaKey103.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema103),
      Request[IO](Method.GET, schemaKey100.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString))),
      Request[IO](Method.GET, schemaKey101.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString))),
      Request[IO](Method.GET, schemaKey102.uri).withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
    )

    val (responses, _) = SchemaServiceSpec.state(reqs, false).unsafeRunSync()

    responses.reverse match {
      case r102 :: r101 :: r100 :: _ =>
        (
          for {
            s100 <- r100.as[Json]
            s101 <- r101.as[Json]
            s102 <- r102.as[Json]
          } yield (s100 must beEqualTo(expectedSchema100))
            .and(s101 must beEqualTo(expectedSchema101))
            .and(s102 must beEqualTo(expectedSchema102))
        ).unsafeRunSync()
      case _ => ko
    }
  }

  def e20 = {
    val (schema100, schemaKey100) = testSchema(SchemaVer.Full(1, 0, 0))
    val (schema101, schemaKey101) = testSchema(SchemaVer.Full(1, 0, 1))
    val (schema101SupersededBySmaller, _) = testSchema(
      version = SchemaVer.Full(1, 0, 1),
      supersedingInfo = Schema.SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 0)).some
    )
    val (schema102Supersedes, schemaKey102) = testSchema(
      version = SchemaVer.Full(1, 0, 2),
      supersedingInfo = Schema
        .SupersedingInfo
        .Superseded(
          NonEmptyList.of(
            SchemaVer.Full(1, 0, 0),
            SchemaVer.Full(1, 0, 3)
          )
        )
        .some
    )

    val reqs = List(
      Request[IO](Method.PUT, schemaKey100.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema100),
      Request[IO](Method.PUT, schemaKey101.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema101),
      Request[IO](Method.PUT, schemaKey101.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema101SupersededBySmaller),
      Request[IO](Method.PUT, schemaKey102.uri)
        .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
        .withEntity(schema102Supersedes)
    )

    val expectedResponseBodies = List(
      json"""
        {
          "message" : "Schema created",
          "updated" : false,
          "location" : "iglu:com.acme/nonexistent/jsonschema/1-0-0",
          "status" : 201
        }""",
      json"""
        {
          "message" : "Schema created",
          "updated" : false,
          "location" : "iglu:com.acme/nonexistent/jsonschema/1-0-1",
          "status" : 201
        }""",
      json"""{"message" : "There are superseded schema versions greater than superseding schema version"}""",
      json"""{"message" : "There are superseded schema versions greater than superseding schema version"}"""
    )

    val expectedResponseStatus = List(Status.Created, Status.Created, Status.Conflict, Status.Conflict)

    val matchStatement = for {
      r <- SchemaServiceSpec.state(reqs, false)
      (responses, _) = r
      responseBodies <- responses.map(_.as[Json]).sequence
      responseStatus = responses.map(_.status)
    } yield (responseBodies must beEqualTo(expectedResponseBodies))
      .and(responseStatus must beEqualTo(expectedResponseStatus))

    matchStatement.unsafeRunSync()
  }
}

object SchemaServiceSpec {
  import storage.InMemory

  implicit class SchemaKeyUri(schemaKey: SchemaKey) {
    def uri: Uri = Uri.unsafeFromString(schemaKey.toPath)
  }

  val client: Client[IO] = Client.fromHttpApp(HttpApp[IO](r => Response[IO]().withEntity(r.body).pure[IO]))

  def request(reqs: List[Request[IO]], patchesAllowed: Boolean): IO[Response[IO]] =
    for {
      storage <- InMemory.getInMemory[IO](SpecHelpers.exampleState)
      service = SchemaService.asRoutes(patchesAllowed, Webhook.WebhookClient(List(), client))(
        storage,
        None,
        SpecHelpers.ctx,
        createRhoMiddleware()
      )
      responses <- reqs.traverse(service.run).value
    } yield responses.flatMap(_.lastOption).getOrElse(Response(Status.NotFound))

  def state(reqs: List[Request[IO]], patchesAllowed: Boolean): IO[(List[Response[IO]], InMemory.State)] =
    for {
      storage <- InMemory.getInMemory[IO](SpecHelpers.exampleState)
      service = SchemaService.asRoutes(patchesAllowed, Webhook.WebhookClient(List(), client))(
        storage,
        None,
        SpecHelpers.ctx,
        createRhoMiddleware()
      )
      responses <- reqs.traverse(service.run).value
      state     <- storage.ref.get
    } yield (responses.getOrElse(List.empty), state)
}
