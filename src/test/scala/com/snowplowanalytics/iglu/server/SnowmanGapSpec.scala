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

import scala.concurrent.duration.DurationInt

import cats.implicits._
import cats.effect.{IO, Resource}
import cats.effect.testing.specs2.CatsEffect

import io.circe.Json
import io.circe.literal._

import org.http4s._
import org.http4s.client.Client
import org.http4s.circe._
import org.http4s.implicits._

import org.specs2.Specification
import org.specs2.execute.Result

import com.snowplowanalytics.iglu.server.middleware.CachingMiddleware
import com.snowplowanalytics.iglu.server.storage.InMemory

/**
  * Tests migrated from Snowman integration test suite (gap analysis).
  * These cover behaviors that were validated by Snowman infrastructure tests
  * but had no equivalent unit test in the iglu-server project itself.
  *
  * Snowman test case IDs: TC-Iglu-003, 004, 015, 016, 019, 020
  */
class SnowmanGapSpec extends Specification with CatsEffect {

  def is = sequential ^ s2"""
  Snowman migration — unit tests
    TC-Iglu-003: Read-only mode server serves public schema lookups          $tcIglu003
    TC-Iglu-004: Read-only mode server rejects schema write requests         $tcIglu004
    TC-Iglu-015: Schema with $$ref is stored and retrieved as-is             $tcIglu015
    TC-Iglu-016: Schema with circular $$ref definitions does not crash       $tcIglu016
    TC-Iglu-019: Meta server endpoint reports accurate schema count          $tcIglu019
    TC-Iglu-020: Accept-Language header does not affect schema responses     $tcIglu020
  """

  val MaxPayloadSize = 100000L
  val MaxJsonDepth   = 20
  val SelfDesc       = "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#"

  val httpClient: Client[IO] = Client.fromHttpApp(HttpApp[IO](r => Response[IO]().withEntity(r.body).pure[IO]))

  def app(
    superKey: Option[java.util.UUID] = None,
    patchesAllowed: Boolean = false
  ): Resource[IO, HttpApp[IO]] =
    for {
      cache   <- CachingMiddleware.initResponseCache[IO](1000, Server.CacheTtl)
      storage <- Resource.eval(InMemory.getInMemory[IO](SpecHelpers.exampleState))
    } yield Server.httpApp(
      storage,
      superKey,
      debug = false,
      patchesAllowed,
      Webhook.WebhookClient(Nil, httpClient),
      cache,
      Config.Swagger(""),
      IO.pure(true),
      Config.Hsts(false, 365.days),
      MaxPayloadSize,
      MaxJsonDepth
    )

  /**
    * TC-Iglu-003: Self-signed / read-only server serves public schema lookups without 403.
    *
    * When no superApiKey is configured the server operates in a de-facto read-only mode.
    * Public schemas must still be retrievable via GET.
    */
  def tcIglu003: IO[Result] =
    app(superKey = None).use { httpApp =>
      val req = Request[IO](Method.GET, uri"/api/schemas/com.acme/event/jsonschema/1-0-0")
      for {
        resp <- httpApp.run(req)
        body <- resp.as[Json]
      } yield {
        (resp.status must beEqualTo(Status.Ok))
          .and(body must beEqualTo(SpecHelpers.selfSchemaZero))
      }
    }

  /**
    * TC-Iglu-004: Self-signed / read-only server rejects write requests.
    *
    * Without a superApiKey and without any stored API key the caller gets Permission.Noop,
    * which cannot create schemas. The server must respond with 403.
    */
  def tcIglu004: IO[Result] =
    app(superKey = None).use { httpApp =>
      val schema = s"""{"type":"object"}"""
      val req = Request[IO](Method.PUT, uri"/api/schemas/com.acme/new/jsonschema/1-0-0?isPublic=true")
        .withEntity(schema)
      for {
        resp <- httpApp.run(req)
        _    <- resp.body.compile.drain
      } yield resp.status must beEqualTo(Status.Forbidden)
    }

  /**
    * TC-Iglu-015: Schema with $ref is stored and retrieved as-is.
    *
    * Upload schema B (a reusable definition), then schema A that references B via a $ref.
    * GET schema A and verify the $ref is preserved — iglu-server stores schemas verbatim
    * and does not inline or resolve cross-schema $ref on retrieval.
    */
  def tcIglu015: IO[Result] = {
    val schemaB =
      s"""{"$$schema":"$SelfDesc","self":{"vendor":"com.acme","name":"ref_target","format":"jsonschema","version":"1-0-0"},"type":"object","description":"Reusable schema B","properties":{"name":{"type":"string"}}}"""

    // Schema A references B through a standard JSON Schema $ref in definitions,
    // plus an Iglu-style reference in a property description to demonstrate cross-schema linking.
    val schemaA =
      json"""{
        "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
        "self": {"vendor": "com.acme", "name": "ref_source", "format": "jsonschema", "version": "1-0-0"},
        "type": "object",
        "description": "Schema A with ref to B",
        "properties": {
          "local_ref": {"$$ref": "#/definitions/reusable"},
          "external_note": {"type": "string", "description": "See iglu:com.acme/ref_target/jsonschema/1-0-0"}
        },
        "definitions": {
          "reusable": {"type": "string", "minLength": 1}
        }
      }"""

    app(superKey = Some(SpecHelpers.superKey)).use { httpApp =>
      val putB = Request[IO](Method.PUT, uri"/api/schemas/com.acme/ref_target/jsonschema/1-0-0?isPublic=true")
        .withHeaders(Headers("apikey" -> SpecHelpers.superKey.toString))
        .withEntity(schemaB)
      val putA = Request[IO](Method.PUT, uri"/api/schemas/com.acme/ref_source/jsonschema/1-0-0?isPublic=true")
        .withHeaders(Headers("apikey" -> SpecHelpers.superKey.toString))
        .withEntity(schemaA)
      val getA = Request[IO](Method.GET, uri"/api/schemas/com.acme/ref_source/jsonschema/1-0-0")

      for {
        respB    <- httpApp.run(putB)
        _        <- respB.body.compile.drain
        respA    <- httpApp.run(putA)
        _        <- respA.body.compile.drain
        respGet  <- httpApp.run(getA)
        bodyJson <- respGet.as[Json]
      } yield {
        // Both uploads succeed
        val uploadB = respB.status.code must beOneOf(200, 201)
        val uploadA = respA.status.code must beOneOf(200, 201)
        // Schema A is retrievable
        val getOk = respGet.status must beEqualTo(Status.Ok)
        // The $ref is preserved as-is in the response (not resolved/inlined)
        val refPreserved =
          bodyJson.hcursor
            .downField("properties")
            .downField("local_ref")
            .downField("$ref")
            .as[String] must beRight("#/definitions/reusable")
        // The definitions block is preserved
        val defsPreserved =
          bodyJson.hcursor.downField("definitions").downField("reusable").downField("type").as[String] must beRight(
            "string"
          )

        uploadB.and(uploadA).and(getOk).and(refPreserved).and(defsPreserved)
      }
    }
  }

  /**
    * TC-Iglu-016: Circular $ref definitions are handled gracefully.
    *
    * A schema with definitions that form a cycle (A -> B -> A) must not cause
    * the server to hang or crash. The server may accept (store as-is) or reject
    * the schema — either outcome is acceptable as long as the response is timely.
    */
  def tcIglu016: IO[Result] = {
    val circularSchema =
      json"""{
        "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
        "self": {"vendor": "com.acme", "name": "circular", "format": "jsonschema", "version": "1-0-0"},
        "type": "object",
        "description": "Schema with circular ref definitions",
        "properties": {
          "node": {"$$ref": "#/definitions/nodeA"}
        },
        "definitions": {
          "nodeA": {"type": "object", "properties": {"child": {"$$ref": "#/definitions/nodeB"}}},
          "nodeB": {"type": "object", "properties": {"parent": {"$$ref": "#/definitions/nodeA"}}}
        }
      }"""

    app(superKey = Some(SpecHelpers.superKey)).use { httpApp =>
      val putReq = Request[IO](Method.PUT, uri"/api/schemas/com.acme/circular/jsonschema/1-0-0?isPublic=true")
        .withHeaders(Headers("apikey" -> SpecHelpers.superKey.toString))
        .withEntity(circularSchema)

      for {
        resp <- httpApp.run(putReq)
        _    <- resp.body.compile.drain
      } yield {
        // The server must respond (not hang) — accept or reject is both fine
        resp.status.code must beBetween(200, 499)
      }
    }
  }

  /**
    * TC-Iglu-019: The /api/meta/server endpoint reports accurate schema count.
    *
    * After publishing a new schema the schemaCount field returned by the
    * meta endpoint must increase accordingly.
    */
  def tcIglu019: IO[Result] =
    app(superKey = Some(SpecHelpers.superKey)).use { httpApp =>
      val getServer = Request[IO](Method.GET, uri"/api/meta/server")
        .withHeaders(Headers("apikey" -> SpecHelpers.superKey.toString))

      val newSchema = s"""{"type":"object"}"""
      val putSchema =
        Request[IO](Method.PUT, uri"/api/schemas/com.acme/counter_test/jsonschema/1-0-0?isPublic=true")
          .withHeaders(Headers("apikey" -> SpecHelpers.superKey.toString))
          .withEntity(newSchema)

      for {
        resp1  <- httpApp.run(getServer)
        body1  <- resp1.as[Json]
        count1  = body1.hcursor.downField("schemaCount").as[Int].getOrElse(-1)

        putResp <- httpApp.run(putSchema)
        _       <- putResp.body.compile.drain

        resp2  <- httpApp.run(getServer)
        body2  <- resp2.as[Json]
        count2  = body2.hcursor.downField("schemaCount").as[Int].getOrElse(-1)
      } yield {
        (resp1.status must beEqualTo(Status.Ok))
          .and(resp2.status must beEqualTo(Status.Ok))
          // The super key can read all schemas; count must increase by 1
          .and(count2 must beEqualTo(count1 + 1))
      }
    }

  /**
    * TC-Iglu-020: Accept-Language and other content-negotiation headers do not
    * affect schema responses.
    *
    * The server always returns application/json regardless of Accept-Language,
    * and the schema body is identical across all language preferences.
    */
  def tcIglu020: IO[Result] =
    app().use { httpApp =>
      val base = Request[IO](Method.GET, uri"/api/schemas/com.acme/event/jsonschema/1-0-0")

      val reqNoLang = base
      val reqEnUS   = base.withHeaders(Headers("Accept-Language" -> "en-US"))
      val reqDeDE   = base.withHeaders(Headers("Accept-Language" -> "de-DE"))
      val reqJaJP   = base.withHeaders(Headers("Accept-Language" -> "ja-JP"))

      for {
        resp0 <- httpApp.run(reqNoLang)
        body0 <- resp0.as[Json]
        resp1 <- httpApp.run(reqEnUS)
        body1 <- resp1.as[Json]
        resp2 <- httpApp.run(reqDeDE)
        body2 <- resp2.as[Json]
        resp3 <- httpApp.run(reqJaJP)
        body3 <- resp3.as[Json]
      } yield {
        // All requests return 200 with identical JSON
        (resp0.status must beEqualTo(Status.Ok))
          .and(resp1.status must beEqualTo(Status.Ok))
          .and(resp2.status must beEqualTo(Status.Ok))
          .and(resp3.status must beEqualTo(Status.Ok))
          .and(body1 must beEqualTo(body0))
          .and(body2 must beEqualTo(body0))
          .and(body3 must beEqualTo(body0))
          // Content-Type is always application/json
          .and(resp1.contentType.map(_.mediaType) must beSome(MediaType.application.json))
          .and(resp2.contentType.map(_.mediaType) must beSome(MediaType.application.json))
      }
    }
}
