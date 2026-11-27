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

import java.util.UUID
import scala.concurrent.duration.DurationInt

import cats.implicits._
import cats.effect.{IO, Resource}
import cats.effect.testing.specs2.CatsEffect

import io.circe.Json

import org.http4s._
import org.http4s.client.Client
import org.http4s.circe._
import org.http4s.implicits._

import org.specs2.Specification
import org.specs2.execute.Result

import com.snowplowanalytics.iglu.server.middleware.CachingMiddleware
import com.snowplowanalytics.iglu.server.storage.{Postgres, Storage}

/**
  * Integration tests migrated from Snowman test suite (gap analysis).
  * These run against a real PostgreSQL database and cover behaviors that
  * require persistent storage to validate.
  *
  * Snowman test case IDs: TC-Iglu-003, 004, 005
  *
  * Prerequisites: PostgreSQL running at localhost:5432 with database "testdb",
  * user "postgres", password "iglusecret".
  *
  * Start with:
  *   docker-compose -f docker/docker-compose.yml up -d postgres
  */
class SnowmanGapSpecPostgres extends Specification with CatsEffect {

  def is = sequential ^ s2"""
  Snowman migration — integration tests (Postgres)
    TC-Iglu-003: Read-only mode serves public schema lookups (Postgres)      $tcIglu003
    TC-Iglu-004: Read-only mode rejects write requests (Postgres)            $tcIglu004
    TC-Iglu-005: Lookup of schema absent from local storage returns 404      $tcIglu005
  """

  val MaxPayloadSize = 100000L
  val MaxJsonDepth   = 20

  val dbPoolConfig = Config.StorageConfig.ConnectionPool.Hikari(None, None, None, None, Config.ThreadPool.Cached)
  val storageConfig =
    Config
      .StorageConfig
      .Postgres("localhost", 5432, "testdb", "postgres", "iglusecret", "org.postgresql.Driver", None, dbPoolConfig, true)

  val httpClient: Client[IO] = Client.fromHttpApp(HttpApp[IO](r => Response[IO]().withEntity(r.body).pure[IO]))

  /**
    * Build a full HttpApp backed by Postgres.
    * Each test gets a freshly bootstrapped database with the standard example state.
    */
  def appResource(superKey: Option[UUID]): Resource[IO, HttpApp[IO]] =
    for {
      storage <- Storage.initialize[IO](storageConfig)
      _       <- Resource.eval(storage.asInstanceOf[Postgres[IO]].drop)
      _       <- Resource.eval(storage.asInstanceOf[Postgres[IO]].bootstrap)
      _ <- Resource.eval(
        SpecHelpers.exampleState.permission.toList
          .map { case (key, perm) => storage.addPermission(key, perm) }
          .sequence
      )
      _ <- Resource.eval(
        SpecHelpers.exampleState.schemas.values.toList
          .map(schema =>
            storage.upsertSchema(schema.schemaMap, schema.body, schema.metadata.isPublic, List.empty, patchesAllowed = false)
          )
          .sequence
      )
      cache <- CachingMiddleware.initResponseCache[IO](1000, Server.CacheTtl)
    } yield Server.httpApp(
      storage,
      superKey,
      debug = false,
      patchesAllowed = false,
      Webhook.WebhookClient(Nil, httpClient),
      cache,
      Config.Swagger(""),
      IO.pure(true),
      Config.Hsts(false, 365.days),
      MaxPayloadSize,
      MaxJsonDepth
    )

  /**
    * TC-Iglu-003 (Postgres): Read-only mode serves public schema lookups.
    *
    * Same assertion as the in-memory unit test, but against a real Postgres backend.
    * Confirms that the permission model works identically with persistent storage.
    */
  def tcIglu003: IO[Result] =
    appResource(superKey = None).use { httpApp =>
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
    * TC-Iglu-004 (Postgres): Read-only mode rejects write requests.
    *
    * Without superApiKey the caller cannot create schemas, even with Postgres storage.
    */
  def tcIglu004: IO[Result] =
    appResource(superKey = None).use { httpApp =>
      val schema = s"""{"type":"object"}"""
      val req = Request[IO](Method.PUT, uri"/api/schemas/com.acme/new/jsonschema/1-0-0?isPublic=true")
        .withEntity(schema)
      for {
        resp <- httpApp.run(req)
        _    <- resp.body.compile.drain
      } yield resp.status must beEqualTo(Status.Forbidden)
    }

  /**
    * TC-Iglu-005: Lookup of a schema absent from local storage returns 404.
    *
    * The Snowman test validates that a resolver can distinguish between present
    * and absent schemas when backed by a remote registry (Iglu Central).
    * iglu-server does not proxy to remote registries — it only serves schemas
    * from its own database. This test confirms that a GET for a known Iglu Central
    * schema (e.g. com.snowplowanalytics.snowplow/unstruct_event) returns 404
    * when that schema has not been published to this server instance.
    *
    * If remote registry proxying is added in the future, this test should be
    * updated to assert 200 for known remote schemas and 404 for truly absent ones.
    */
  def tcIglu005: IO[Result] =
    appResource(superKey = None).use { httpApp =>
      val knownRemote =
        Request[IO](Method.GET, uri"/api/schemas/com.snowplowanalytics.snowplow/unstruct_event/jsonschema/1-0-0")
      val unknownSchema =
        Request[IO](Method.GET, uri"/api/schemas/com.nonexistent/never_published/jsonschema/1-0-0")

      for {
        resp1 <- httpApp.run(knownRemote)
        _     <- resp1.body.compile.drain
        resp2 <- httpApp.run(unknownSchema)
        _     <- resp2.body.compile.drain
      } yield {
        // Both return 404 — server has no remote fallback
        (resp1.status must beEqualTo(Status.NotFound))
          .and(resp2.status must beEqualTo(Status.NotFound))
      }
    }
}
