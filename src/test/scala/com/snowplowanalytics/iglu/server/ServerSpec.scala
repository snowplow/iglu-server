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

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.{ContextShift, IO, Resource, Timer}

import io.circe.Json
import io.circe.literal._

import org.http4s._
import org.http4s.implicits._
import org.http4s.circe._
import org.http4s.client.blaze.BlazeClientBuilder

import org.specs2.Specification

import com.snowplowanalytics.iglu.core.{SchemaMap, SchemaVer, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.implicits._

import com.snowplowanalytics.iglu.server.storage.{Postgres, Storage}
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Permission}
import com.snowplowanalytics.iglu.server.model.Schema.SupersedingInfo
import com.snowplowanalytics.iglu.server.storage.InMemory
import com.snowplowanalytics.iglu.server.codecs.JsonCodecs._
import com.snowplowanalytics.iglu.server.model.SchemaSpec.testSchema
import com.snowplowanalytics.iglu.server.service.SchemaServiceSpec._

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
  Create schemas with superseding info correctly $e5
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
    val (schema100, schemaKey100) = testSchema(SchemaVer.Full(1, 0, 0))
    val (schema101, schemaKey101) = testSchema(SchemaVer.Full(1, 0, 1))
    val (schema102, schemaKey102) = testSchema(
      version = SchemaVer.Full(1, 0, 2),
      supersedingInfo = SupersedingInfo
        .Superseded(
          NonEmptyList.of(
            SchemaVer.Full(1, 0, 1)
          )
        )
        .some
    )
    val (schema103, schemaKey103) = testSchema(
      version = SchemaVer.Full(1, 0, 3),
      supersedingInfo = SupersedingInfo
        .Superseded(
          NonEmptyList.of(
            SchemaVer.Full(1, 0, 2)
          )
        )
        .some
    )
    val (schema100SupersededBy, _) = testSchema(
      version = SchemaVer.Full(1, 0, 0),
      supersedingInfo = SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 1)).some
    )
    val (expectedSchema100, _) = testSchema(
      version = SchemaVer.Full(1, 0, 0),
      supersedingInfo = SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 3)).some
    )
    val (expectedSchema101, _) = testSchema(
      version = SchemaVer.Full(1, 0, 1),
      supersedingInfo = SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 3)).some
    )
    val (expectedSchema102, _) = testSchema(
      version = SchemaVer.Full(1, 0, 2),
      supersedingInfo = SupersedingInfo.SupersededBy(SchemaVer.Full(1, 0, 3)).some
    )
    val (expectedSchema103, _) = testSchema(version = SchemaVer.Full(1, 0, 3))

    val reqs = List(
      Request[IO](Method.POST, uri"/")
        .withEntity(schema100)
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.POST, uri"/")
        .withEntity(schema101)
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.POST, uri"/")
        .withEntity(schema102)
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.POST, uri"/")
        .withEntity(schema103)
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.POST, uri"/")
        .withEntity(schema100SupersededBy)
        .withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.GET, schemaKey100.uri).withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.GET, schemaKey101.uri).withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.GET, schemaKey102.uri).withHeaders(Header("apikey", InMemory.DummySuperKey.toString)),
      Request[IO](Method.GET, schemaKey103.uri).withHeaders(Header("apikey", InMemory.DummySuperKey.toString))
    )

    val expected = List(
      TestResponse(
        201,
        json"""{"message": "Schema created", "updated": false, "location": "iglu:com.acme/nonexistent/jsonschema/1-0-0", "status": 201}"""
      ),
      TestResponse(
        201,
        json"""{"message": "Schema created", "updated": false, "location": "iglu:com.acme/nonexistent/jsonschema/1-0-1", "status": 201}"""
      ),
      TestResponse(
        201,
        json"""{"message": "Schema created", "updated": false, "location": "iglu:com.acme/nonexistent/jsonschema/1-0-2", "status": 201}"""
      ),
      TestResponse(
        201,
        json"""{"message": "Schema created", "updated": false, "location": "iglu:com.acme/nonexistent/jsonschema/1-0-3", "status": 201}"""
      ),
      TestResponse(
        200,
        json"""{"message": "Schema updated", "updated": true, "location": "iglu:com.acme/nonexistent/jsonschema/1-0-0", "status": 200}"""
      ),
      TestResponse(200, expectedSchema100),
      TestResponse(200, expectedSchema101),
      TestResponse(200, expectedSchema102),
      TestResponse(200, expectedSchema103)
    )

    val action = for {
      responses <- ServerSpec.executeRequests(reqs)
      results   <- responses.traverse(res => TestResponse.build[Json](res))
    } yield results

    execute(action) must beEqualTo(expected)
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
  val httpConfig = Config.Http("0.0.0.0", 8080, None, None, Config.ThreadPool.Cached)
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
  val config = Config(storageConfig, httpConfig, false, true, Nil, Config.Swagger(""), None, 10.seconds, false)

  private val runServer = Server.buildServer(config, IO.pure(true)).flatMap(_.resource)
  private val client    = BlazeClientBuilder[IO](global).resource
  private val env       = client <* runServer

  /** Execute requests against fresh server (only one execution per test is allowed) */
  def executeRequests(requests: List[Request[IO]]): IO[List[Response[IO]]] = {
    val r = requests.map { r =>
      if (r.uri.host.isDefined) r
      else r.withUri(uri"http://localhost:8080/api/schemas".addPath(r.uri.path))
    }
    env.use(client => r.traverse(client.run(_).use(IO.pure)))
  }

  val specification = Resource.make {
    Storage.initialize[IO](storageConfig).use(s => s.asInstanceOf[Postgres[IO]].drop) *>
      Server.setup(ServerSpec.config, None).void *>
      Storage.initialize[IO](storageConfig).use(_.addPermission(InMemory.DummySuperKey, Permission.Super))
  }(_ => Storage.initialize[IO](storageConfig).use(s => s.asInstanceOf[Postgres[IO]].drop))

  def execute[A](action: IO[A]): A =
    specification.use(_ => action).unsafeRunSync()

  case class TestResponse[E](status: Int, body: E)

  object TestResponse {
    def build[E](actual: Response[IO])(implicit decoder: EntityDecoder[IO, E]): IO[TestResponse[E]] =
      actual.as[E].map(body => TestResponse(actual.status.code, body))
  }
}
