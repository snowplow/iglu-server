/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.0
 * located at https://docs.snowplow.io/limited-use-license-1.0
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server

import cats.implicits._
import cats.effect.{ContextShift, IO, Resource, Timer}

import io.circe.Json
import io.circe.literal._

import org.http4s._
import org.http4s.implicits._
import org.http4s.circe._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.headers.`Strict-Transport-Security`

import org.specs2.Specification

import com.snowplowanalytics.iglu.core.{SchemaMap, SchemaVer, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.implicits._

import com.snowplowanalytics.iglu.server.storage.{Postgres, Storage}
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Permission}
import com.snowplowanalytics.iglu.server.storage.InMemory
import com.snowplowanalytics.iglu.server.codecs.JsonCodecs._

import scala.concurrent.duration.DurationLong

// Integration test requiring a database
// docker run --name igludb -e POSTGRES_PASSWORD=iglusecret -e POSTGRES_DB=testdb -p 5432:5432 -d postgres
class ServerSpec extends Specification {
  def is = sequential ^ s2"""
  ${action(System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "off"))}
  Return 404 for non-existing schema $e1
  Return 404 for unknown endpoint $e2
  Create a new private schema via PUT, return it with proper apikey, hide for no apikey $e3
  Create a new public schema via POST, get it from /schemas, delete it $e4
  Return an HSTS header when configured to do so $e5
  ${action(System.clearProperty("org.slf4j.simpleLogger.defaultLogLevel"))}
  """
  import ServerSpec._

  def e1 = {
    val req      = Request[IO](Method.GET, uri"/com.acme/event/jsonschema/1-0-0")
    val expected = TestResponse(404, IgluResponse.SchemaNotFound)

    val action = for {
      responses <- ServerSpec.executeRequests(List(req))
      results   <- responses.traverse(res => TestResponse.build[IgluResponse](res))
    } yield results

    execute(action) must beEqualTo(List(expected))
  }

  def e2 = {
    val req      = Request[IO](Method.GET, uri"http://localhost:8080/boom")
    val expected = TestResponse(404, IgluResponse.Message("The endpoint does not exist"))

    val action = for {
      responses <- ServerSpec.executeRequests(List(req))
      results   <- responses.traverse(res => TestResponse.build[IgluResponse](res))
    } yield results

    execute(action) must beEqualTo(List(expected))
  }

  def e3 = {
    val reqs = List(
      Request[IO](Method.PUT, uri"/com.acme/first/jsonschema/1-0-0")
        .withEntity(json"""{"properties": {}}""")
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.GET, uri"/").withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.GET, uri"/com.acme/first/jsonschema/1-0-0")
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.GET, uri"/com.acme/first/jsonschema/1-0-0")
    )

    val expected = List(
      TestResponse(
        201,
        json"""{"message": "Schema created", "updated": false, "location": "iglu:com.acme/first/jsonschema/1-0-0", "status": 201}"""
      ),
      TestResponse(200, json"""["iglu:com.acme/first/jsonschema/1-0-0"]"""),
      TestResponse(
        200,
        json"""{
        "$$schema" : "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
        "self": {"vendor": "com.acme", "name": "first", "format": "jsonschema", "version": "1-0-0"},
        "properties" : {}}"""
      ),
      TestResponse(404, json"""{"message" : "The schema is not found"}""")
    )

    val action = for {
      responses <- ServerSpec.executeRequests(reqs)
      results   <- responses.traverse(res => TestResponse.build[Json](res))
    } yield results

    execute(action) must beEqualTo(expected)
  }

  def e4 = {
    val schema = SelfDescribingSchema[Json](
      SchemaMap("com.acme", "first", "jsonschema", SchemaVer.Full(1, 0, 0)),
      json"""{"properties": {}}"""
    ).normalize

    val reqs = List(
      Request[IO](Method.POST, uri"/".withQueryParam("isPublic", "true"))
        .withEntity(schema)
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.DELETE, uri"/com.acme/first/jsonschema/1-0-0")
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.GET, uri"/")
    )

    val expected = List(
      TestResponse(
        201,
        json"""{"message": "Schema created", "updated": false, "location": "iglu:com.acme/first/jsonschema/1-0-0", "status": 201}"""
      ),
      TestResponse(200, json"""{"message":"Schema deleted"}"""),
      TestResponse(200, json"""[]""")
    )

    val action = for {
      responses <- ServerSpec.executeRequests(reqs)
      results   <- responses.traverse(res => TestResponse.build[Json](res))
    } yield results

    execute(action) must beEqualTo(expected)
  }

  def e5 = {
    val schema = SelfDescribingSchema[Json](
      SchemaMap("com.acme", "first", "jsonschema", SchemaVer.Full(1, 0, 0)),
      json"""{"properties": {}}"""
    ).normalize

    val reqs = List(
      Request[IO](Method.POST, uri"/".withQueryParam("isPublic", "true"))
        .withEntity(schema)
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.DELETE, uri"/com.acme/first/jsonschema/1-0-0")
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.GET, uri"/"),
      Request[IO](Method.GET, uri"/nonExistingEndpoint")
    )

    val expected = Some(
      List(
        `Strict-Transport-Security`.unsafeFromLong(180 * 24 * 3600, includeSubDomains = true),
        `Strict-Transport-Security`.unsafeFromLong(180 * 24 * 3600, includeSubDomains = true),
        `Strict-Transport-Security`.unsafeFromLong(180 * 24 * 3600, includeSubDomains = true),
        `Strict-Transport-Security`.unsafeFromLong(180 * 24 * 3600, includeSubDomains = true)
      )
    )

    def action(hsts: Config.Hsts) =
      for {
        responses <- ServerSpec.executeRequests(reqs, hsts)
        results = responses.traverse(res => res.headers.get(`Strict-Transport-Security`))
      } yield results

    val hstsOn  = Config.Hsts(enable = true, 180.days)
    val hstsOff = Config.Hsts(enable = false, 365.days)

    val on  = execute(action(hstsOn), hstsOn) must beEqualTo(expected)
    val off = execute(action(hstsOff), hstsOff) must beEqualTo(None)

    on.and(off)
  }
}

object ServerSpec {
  import scala.concurrent.ExecutionContext.global
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO]     = IO.timer(global)

  val dbPoolConfig = Config
    .StorageConfig
    .ConnectionPool
    .Hikari(None, None, None, None, Config.ThreadPool.Cached, Config.ThreadPool.Cached)
  def httpConfig(hsts: Config.Hsts) = Config.Http("0.0.0.0", 8080, None, None, Config.ThreadPool.Cached, hsts)
  val storageConfig =
    Config
      .StorageConfig
      .Postgres(
        "localhost",
        5432,
        "testdb",
        "postgres",
        "iglusecret",
        "org.postgresql.Driver",
        None,
        dbPoolConfig,
        true
      )
  def config(hsts: Config.Hsts) =
    Config(
      storageConfig,
      httpConfig(hsts),
      false,
      true,
      Nil,
      Config.Swagger(""),
      None,
      10.seconds,
      false,
      Config.License(true)
    )

  private def runServer(hsts: Config.Hsts) = Server.buildServer(config(hsts), IO.pure(true)).flatMap(_.resource)
  private val client                       = BlazeClientBuilder[IO](global).resource
  private def env(hsts: Config.Hsts)       = client <* runServer(hsts)

  /** Execute requests against fresh server (only one execution per test is allowed) */
  def executeRequests(
    requests: List[Request[IO]],
    hsts: Config.Hsts = Config.Hsts(false, 365.days)
  ): IO[List[Response[IO]]] = {
    val r = requests.map { r =>
      if (r.uri.host.isDefined) r
      else r.withUri(uri"http://localhost:8080/api/schemas".addPath(r.uri.path))
    }
    env(hsts).use(client => r.traverse(client.run(_).use(IO.pure)))
  }

  def specification(hsts: Config.Hsts) =
    Resource.make {
      Storage.initialize[IO](storageConfig).use(s => s.asInstanceOf[Postgres[IO]].drop) *>
        Server.setup(ServerSpec.config(hsts), None).void *>
        Storage.initialize[IO](storageConfig).use(_.addPermission(InMemory.DummySuperKey, Permission.Super))
    }(_ => Storage.initialize[IO](storageConfig).use(s => s.asInstanceOf[Postgres[IO]].drop))

  def execute[A](action: IO[A], hsts: Config.Hsts = Config.Hsts(false, 365.days)): A =
    specification(hsts).use(_ => action).unsafeRunSync()

  case class TestResponse[E](status: Int, body: E)

  object TestResponse {
    def build[E](actual: Response[IO])(implicit decoder: EntityDecoder[IO, E]): IO[TestResponse[E]] =
      actual.as[E].map(body => TestResponse(actual.status.code, body))
  }
}
