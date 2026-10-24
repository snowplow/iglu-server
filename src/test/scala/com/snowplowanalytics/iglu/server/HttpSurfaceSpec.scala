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
import io.circe.literal._
import io.circe.parser.{parse => parseJson}

import org.http4s._
import org.http4s.client.Client
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._

import org.typelevel.ci.CIString

import org.specs2.Specification
import org.specs2.execute.Result
import org.specs2.matcher.MustMatchers
import org.specs2.specification.core.{AsExecution, Execution, Fragments}

import com.snowplowanalytics.iglu.core.circe.MetaSchemas

import com.snowplowanalytics.iglu.server.generated.BuildInfo
import com.snowplowanalytics.iglu.server.middleware.CachingMiddleware
import com.snowplowanalytics.iglu.server.storage.InMemory

/**
  * Characterisation of the original HTTP surface before removing rho: route matching, parameter parsing,
  * error bodies and content types, through the full middleware stack of `Server.httpApp`.
  *
  * Business rules (versioning, permissions, superseding) are covered by the service specs. Cases also pin the
  * requests that clients send: Snowman's Iglu Server tests (no `Content-Type`, `Apikey` header, deeply nested bodies)
  * and the Console (`repr=canonical`, by-model lookups, keygen).
  */
class HttpSurfaceSpec extends Specification with CatsEffect {
  import HttpSurfaceSpec._

  def is = sequential ^ s2"""
  HTTP surface stays the same
  ${Fragments.foreach(allCases)(c => fragmentFactory.example(s"${c.id}: ${c.description}", execution(c)) ^ br)}
  """

  def execution(c: Case): Execution =
    AsExecution[IO[Result]].execute(check(c))

  def check(c: Case): IO[Result] =
    run(c).map { response =>
      val status  = response.status must beEqualTo(c.expected.status)
      val ct      = response.contentType must beEqualTo(c.expected.contentType)
      val present = c.expected.headers.toList.map { case (k, v) => response.header(k) must beSome(v) }
      val absent =
        c.expected
          .absentHeaders
          .toList
          .map(k => (response.header(k) must beNone).updateMessage((m: String) => s"$k: $m"))
      val body = c.expected.body.check(response.body)
      Result
        .foreach(present ++ absent)(identity)
        .and(status)
        .and(ct)
        .and(body)
        .mapMessage((m: String) =>
          if (c.setup.isEmpty) m else s"$m (setup statuses: ${response.setupStatuses.mkString(", ")})"
        )
    }

  val httpClient: Client[IO] = Client.fromHttpApp(HttpApp[IO](r => Response[IO]().withEntity(r.body).pure[IO]))

  def app(debug: Boolean, patchesAllowed: Boolean): Resource[IO, HttpApp[IO]] =
    for {
      cache   <- CachingMiddleware.initResponseCache[IO](1000, Server.CacheTtl)
      storage <- Resource.eval(InMemory.getInMemory[IO](SpecHelpers.exampleState))
    } yield Server.httpApp(
      storage,
      None,
      debug,
      patchesAllowed,
      Webhook.WebhookClient(Nil, httpClient),
      cache,
      Config.Swagger(""),
      IO.pure(true),
      Config.Hsts(false, 365.days),
      MaxPayloadSize,
      MaxJsonDepth
    )

  /** Each case gets its own app so writes from one case can't affect another. */
  def run(c: Case): IO[HttpResponse] =
    app(c.debug, c.patchesAllowed).use { a =>
      for {
        setup <- c.setup.traverse(r => a.run(r).flatTap(_.body.compile.drain).map(_.status.code))
        resp  <- a.run(c.request)
        body  <- resp.bodyText.compile.foldMonoid
      } yield HttpResponse(resp.status.code, resp.headers.get[`Content-Type`].map(_.value), resp.headers, body, setup)
    }
}

object HttpSurfaceSpec extends MustMatchers {
  // iglu-core loads its metaschema through the context classloader. Requests may run on pool threads
  // that do not see the test classpath, so load it upfront from the test thread.
  locally(MetaSchemas.JsonSchemaV4Text)

  val SelfDesc = "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#"
  val JsonCT   = Some("application/json")
  val TextCT   = Some("text/plain; charset=UTF-8")

  val MaxPayloadSize = 100000L
  val MaxJsonDepth   = 20

  sealed trait Body {
    def check(actual: String): Result
  }

  object Body {

    /**
      * JSON equality. The elements of a top-level array are compared ignoring order, as the order of listed schemas is a
      * storage concern. Nested arrays, such as `required` in a schema, must keep their order.
      */
    final case class Json(expected: io.circe.Json, normalise: io.circe.Json => io.circe.Json = identity) extends Body {
      def check(actual: String): Result =
        parseJson(actual).map(j => sortTopLevel(normalise(j))) must beRight(sortTopLevel(normalise(expected)))
    }

    final case class Text(expected: String) extends Body {
      def check(actual: String): Result = actual must beEqualTo(expected)
    }

    final case class StartsWith(prefix: String) extends Body {
      def check(actual: String): Result = actual must startWith(prefix)
    }

    final case class Satisfies(description: String, p: io.circe.Json => Boolean) extends Body {
      def check(actual: String): Result =
        (parseJson(actual).exists(p) must beTrue).updateMessage(m => s"$description: $m (body was $actual)")
    }

    private def sortTopLevel(json: io.circe.Json): io.circe.Json =
      json.mapArray(_.sortBy(_.noSpacesSortKeys))
  }

  final case class Expected(
    status: Int,
    contentType: Option[String],
    body: Body,
    headers: Map[String, String] = Map.empty,
    absentHeaders: Set[String] = Set.empty
  )

  object Expected {
    def json(status: Int, body: Json): Expected     = Expected(status, JsonCT, Body.Json(body))
    def message(status: Int, msg: String): Expected = json(status, Json.obj("message" -> Json.fromString(msg)))
  }

  final case class Case(
    id: String,
    description: String,
    request: Request[IO],
    expected: Expected,
    setup: List[Request[IO]] = Nil,
    debug: Boolean = false,
    patchesAllowed: Boolean = false
  )

  final case class HttpResponse(
    status: Int,
    contentType: Option[String],
    headers: Headers,
    body: String,
    setupStatuses: List[Int]
  ) {

    /** Header names are case-insensitive */
    def header(name: String): Option[String] = headers.get(CIString(name)).map(_.head.value)
  }

  def apikey(k: UUID): Header.Raw = Header.Raw(CIString("apikey"), k.toString)
  val superKey: Header.Raw        = apikey(SpecHelpers.superKey)

  def get(uri: Uri, hs: Header.ToRaw*): Request[IO]    = Request[IO](Method.GET, uri).withHeaders(hs: _*)
  def delete(uri: Uri, hs: Header.ToRaw*): Request[IO] = Request[IO](Method.DELETE, uri).withHeaders(hs: _*)
  def put(uri: Uri, body: String, hs: Header.ToRaw*): Request[IO] =
    Request[IO](Method.PUT, uri).withEntity(body).withHeaders(hs: _*)
  def post(uri: Uri, body: String, hs: Header.ToRaw*): Request[IO] =
    Request[IO](Method.POST, uri).withEntity(body).withHeaders(hs: _*)

  val newSchema =
    s"""{"$$schema":"$SelfDesc","self":{"vendor":"com.acme","name":"new","format":"jsonschema","version":"1-0-0"},"type":"object"}"""

  val event100 =
    json"""{"$$schema":$SelfDesc,"self":{"vendor":"com.acme","name":"event","format":"jsonschema","version":"1-0-0"},"type":"object","properties":{"one":{}}}"""

  val secret100 =
    json"""{"$$schema":$SelfDesc,"self":{"vendor":"com.acme","name":"secret","format":"jsonschema","version":"1-0-0"},"type":"object","properties":{"password":{}},"required":["password"]}"""

  val eventUris = json"""["iglu:com.acme/event/jsonschema/1-0-0","iglu:com.acme/event/jsonschema/1-0-1"]"""
  val allUris =
    json"""["iglu:com.acme/event/jsonschema/1-0-1","iglu:com.acme/event/jsonschema/1-0-0","iglu:com.acme.clash/test/jsonschema/1-0-0","iglu:com.acme/clash_test/jsonschema/1-0-0"]"""

  val created =
    json"""{"message":"Schema created","updated":false,"location":"iglu:com.acme/new/jsonschema/1-0-0","status":201}"""

  val event101 =
    json"""{"$$schema":$SelfDesc,"self":{"vendor":"com.acme","name":"event","format":"jsonschema","version":"1-0-1"},"type":"object","properties":{"one":{},"two":{}}}"""

  val fourCanonical = Body.Satisfies(
    "4 canonical schemas",
    _.asArray.exists(a => a.size == 4 && a.forall(_.hcursor.downField("$schema").as[String] == Right(SelfDesc)))
  )

  /** The lint report of `newSchema`, from the validation routes */
  val newSchemaReport =
    json"""{"message":"The schema does not conform to a JSON Schema v4 specification","report":[{"message":"The schema is missing the \"description\" property","level":"INFO","pointer":"/"},{"message":"At the root level, the schema should have a \"type\" property set to \"object\" and have a \"properties\" property","level":"WARNING","pointer":"/"}]}"""

  val badName = newSchema.replace(""""name":"new"""", """"name":"name with spaces"""")

  def reportsBadName(j: Json): Boolean =
    j.hcursor.get[String]("message") == Right("The schema does not conform to a JSON Schema v4 specification") &&
      j.hcursor.downField("report").values.exists(_.exists(_.hcursor.get[String]("pointer") == Right("/self/name")))

  def withoutContentType(r: Request[IO]): Request[IO] = r.removeHeader[`Content-Type`]
  def asJson(r: Request[IO]): Request[IO]             = r.withContentType(`Content-Type`(MediaType.application.json))

  def withoutTimestamps(json: Json): Json =
    json
      .hcursor
      .downField("metadata")
      .withFocus(_.mapObject(_.remove("createdAt").remove("updatedAt")))
      .top
      .getOrElse(json)

  val schemaCases = List(
    Case("s1", "GET a schema", get(uri"/api/schemas/com.acme/event/jsonschema/1-0-0"), Expected.json(200, event100)),
    Case(
      "s2",
      "GET a schema with repr=uri",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0-0?repr=uri"),
      Expected.json(200, Json.fromString("iglu:com.acme/event/jsonschema/1-0-0"))
    ),
    Case(
      "s3",
      "GET a schema with repr=meta",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0-0?repr=meta"),
      Expected.json(
        200,
        json"""{"self":{"vendor":"com.acme","name":"event","format":"jsonschema","version":"1-0-0"},"metadata":{"createdAt":"2018-09-22T12:57:41Z","updatedAt":"2018-09-22T12:57:41Z","isPublic":true},"type":"object","properties":{"one":{}}}"""
      )
    ),
    Case(
      "s4",
      "GET a schema with repr=canonical",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0-0?repr=canonical"),
      Expected.json(200, event100)
    ),
    Case(
      "s5",
      "GET with an invalid SchemaVer",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0-x"),
      Expected.message(400, "Cannot parse version part '1-0-x' as SchemaVer, INVALID_SCHEMAVER")
    ),
    Case(
      "s6",
      "GET with a partial SchemaVer",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0"),
      Expected.message(400, "Cannot parse version part '1-0' as SchemaVer, INVALID_SCHEMAVER")
    ),
    Case(
      "s7",
      "GET with an empty version",
      get(uri"/api/schemas/com.acme/event/jsonschema/"),
      Expected.message(404, "Version cannot be empty, should be model to get SchemaList or full SchemaVer")
    ),
    Case(
      "s8",
      "GET by model when the last segment is an integer and format is jsonschema",
      get(uri"/api/schemas/com.acme/event/jsonschema/1"),
      Expected.json(200, eventUris)
    ),
    Case(
      "s9",
      "GET by model without matching schemas",
      get(uri"/api/schemas/com.acme/event/jsonschema/9"),
      Expected.message(404, "No schemas available")
    ),
    Case(
      "s10",
      "GET with a non-integer, non-SchemaVer last segment",
      get(uri"/api/schemas/com.acme/event/jsonschema/x"),
      Expected.message(400, "Cannot parse version part 'x' as SchemaVer, INVALID_SCHEMAVER")
    ),
    Case(
      "s11",
      "GET by model is only for the jsonschema format",
      get(uri"/api/schemas/com.acme/event/other/1"),
      Expected.message(400, "Cannot parse version part '1' as SchemaVer, INVALID_SCHEMAVER")
    ),
    Case(
      "s12",
      "GET a schema with another format",
      get(uri"/api/schemas/com.acme/event/other/1-0-0"),
      Expected.message(404, "The schema is not found")
    ),
    Case("s13", "GET by vendor and name", get(uri"/api/schemas/com.acme/event"), Expected.json(200, eventUris)),
    Case(
      "s14",
      "GET by vendor",
      get(uri"/api/schemas/com.acme"),
      Expected.json(
        200,
        json"""["iglu:com.acme/event/jsonschema/1-0-0","iglu:com.acme/clash_test/jsonschema/1-0-0","iglu:com.acme/event/jsonschema/1-0-1"]"""
      )
    ),
    Case(
      "s15",
      "GET by vendor without matching schemas",
      get(uri"/api/schemas/com.nope"),
      Expected.message(404, "No schemas available")
    ),
    Case("s16", "GET all schemas", get(uri"/api/schemas"), Expected.json(200, allUris)),
    Case("s17", "GET all schemas with a trailing slash", get(uri"/api/schemas/"), Expected.json(200, allUris)),
    Case(
      "s18",
      "GET all schemas with repr=canonical",
      get(uri"/api/schemas?repr=canonical"),
      Expected(200, JsonCT, fourCanonical)
    ),
    Case(
      "s19",
      "GET all schemas with an unknown repr",
      get(uri"/api/schemas?repr=foo"),
      Expected.message(400, "Unknown schema representation format: 'foo'")
    ),
    Case(
      "s20",
      "GET by vendor and name with an unknown repr",
      get(uri"/api/schemas/com.acme/event?repr=foo"),
      Expected.message(400, "Unknown schema representation format: 'foo'")
    ),
    Case(
      "s21",
      "GET a private schema without apikey",
      get(uri"/api/schemas/com.acme/secret/jsonschema/1-0-0"),
      Expected.message(404, "The schema is not found")
    ),
    Case(
      "s22",
      "GET a private schema with a vendor read key",
      get(uri"/api/schemas/com.acme/secret/jsonschema/1-0-0", apikey(SpecHelpers.readKeyAcme)),
      Expected.json(200, secret100)
    ),
    Case(
      "s23",
      "GET with a malformed apikey",
      get(uri"/api/schemas/com.acme/secret/jsonschema/1-0-0", "apikey" -> "not-a-uuid"),
      Expected.message(400, "Invalid UUID for ApiKey")
    ),
    Case(
      "s24",
      "GET a private schema with an unknown apikey",
      get(uri"/api/schemas/com.acme/secret/jsonschema/1-0-0", apikey(SpecHelpers.nonExistentKey)),
      Expected.message(404, "The schema is not found")
    ),
    Case(
      "s25",
      "DELETE with the super key when patches are not allowed",
      delete(uri"/api/schemas/com.acme/event/jsonschema/1-0-0", superKey),
      Expected.message(405, "DELETE is forbidden on production registry")
    ),
    Case(
      "s26",
      "DELETE without the super key",
      delete(uri"/api/schemas/com.acme/event/jsonschema/1-0-0", apikey(SpecHelpers.readKeyAcme)),
      Expected.message(401, "Not enough permissions")
    ),
    Case(
      "s27",
      "DELETE with an invalid SchemaVer",
      delete(uri"/api/schemas/com.acme/event/jsonschema/1-0-x", superKey),
      Expected.message(400, "Cannot parse version part '1-0-x' as SchemaVer, INVALID_SCHEMAVER")
    ),
    Case(
      "s28",
      "PUT with an invalid isPublic",
      put(uri"/api/schemas/com.acme/new/jsonschema/1-0-0?isPublic=maybe", """{"type":"object"}""", superKey),
      Expected.message(400, "Invalid boolean format: 'maybe'")
    ),
    Case(
      "s29",
      "PUT without a body",
      Request[IO](Method.PUT, uri"/api/schemas/com.acme/new/jsonschema/1-0-0").withHeaders(superKey),
      Expected.message(400, "The request body was malformed.")
    ),
    Case(
      "s30",
      "PUT with a non-JSON body",
      put(uri"/api/schemas/com.acme/new/jsonschema/1-0-0", "not json", superKey),
      Expected.message(400, "The request body was malformed.")
    ),
    Case(
      "s31",
      "PUT a schema body",
      put(uri"/api/schemas/com.acme/new/jsonschema/1-0-0?isPublic=true", """{"type":"object"}""", superKey),
      Expected.json(201, created)
    ),
    Case(
      "s32",
      "PUT a self-describing schema whose self does not match the URI",
      put(uri"/api/schemas/com.acme/other/jsonschema/1-0-0", newSchema, superKey),
      Expected.json(
        400,
        json"""{"uriSchemaKey":"iglu:com.acme/other/jsonschema/1-0-0","payloadSchemaKey":"iglu:com.acme/new/jsonschema/1-0-0","message":"Mismatch: the schema metadata does not match the payload and URI"}"""
      )
    ),
    Case(
      "s33",
      "POST a schema that is not self-describing",
      post(uri"/api/schemas", """{"type":"object"}""", superKey),
      Expected.message(400, "Cannot decode JSON schema")
    ),
    Case(
      "s34",
      "POST a self-describing schema",
      post(uri"/api/schemas?isPublic=true", newSchema, superKey),
      Expected.json(201, created)
    ),
    Case(
      "s35",
      "POST a self-describing schema with a trailing slash",
      post(uri"/api/schemas/", newSchema, superKey),
      Expected.json(201, created)
    ),
    Case(
      "s36",
      "An unsupported method on a known path",
      Request[IO](Method.PATCH, uri"/api/schemas"),
      Expected.message(405, "PATCH not allowed. Defined methods: GET, POST\n")
    ),
    Case(
      "s37",
      "A CORS preflight request",
      Request[IO](Method.OPTIONS, uri"/api/schemas")
        .withHeaders("Origin" -> "http://example.com", "Access-Control-Request-Method" -> "GET"),
      Expected(
        200,
        None,
        Body.Text(""),
        Map(
          "Access-Control-Allow-Origin"  -> "http://example.com",
          "Access-Control-Allow-Methods" -> "OPTIONS, PUT, GET, POST, DELETE",
          "Access-Control-Allow-Headers" -> "content-type, apikey",
          "Access-Control-Max-Age"       -> "86400"
        ),
        Set("Access-Control-Allow-Credentials")
      )
    ),
    Case(
      "s38",
      "Unknown query parameters are ignored",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0-0?repr=uri&unknown=1"),
      Expected.json(200, Json.fromString("iglu:com.acme/event/jsonschema/1-0-0"))
    ),
    Case(
      "s39",
      "The first of repeated query parameters wins",
      get(uri"/api/schemas?repr=canonical&repr=uri"),
      Expected(200, JsonCT, fourCanonical)
    ),
    Case(
      "s40",
      "A schema created by PUT can be fetched",
      get(uri"/api/schemas/com.acme/new/jsonschema/1-0-0"),
      Expected.json(
        200,
        json"""{"$$schema":$SelfDesc,"self":{"vendor":"com.acme","name":"new","format":"jsonschema","version":"1-0-0"},"type":"object"}"""
      ),
      setup =
        List(put(uri"/api/schemas/com.acme/new/jsonschema/1-0-0?isPublic=true", """{"type":"object"}""", superKey))
    ),
    Case(
      "s41",
      "repr is case-insensitive",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0-0?repr=URI"),
      Expected.json(200, Json.fromString("iglu:com.acme/event/jsonschema/1-0-0"))
    ),
    Case(
      "s42",
      "An empty apikey header is malformed",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0-0", "apikey" -> ""),
      Expected.message(400, "Invalid UUID for ApiKey")
    ),
    Case(
      "s43",
      "GET by model with repr=canonical",
      get(uri"/api/schemas/com.acme/event/jsonschema/1?repr=canonical"),
      Expected.json(200, Json.arr(event100, event101))
    ),
    Case(
      "s44",
      "POST a schema that skips a SchemaVer",
      post(
        uri"/api/schemas?isPublic=true",
        s"""{"$$schema":"$SelfDesc","self":{"vendor":"com.acme","name":"event","format":"jsonschema","version":"1-0-3"},"type":"object"}""",
        superKey
      ),
      Expected.message(409, "Preceding SchemaVer in the group is missing, check that schemas published in proper order")
    ),
    Case(
      "s45",
      "POST a schema whose {vendor}_{name} clashes with existing schemas",
      post(
        uri"/api/schemas?isPublic=true",
        s"""{"$$schema":"$SelfDesc","self":{"vendor":"com.acme","name":"clash-test","format":"jsonschema","version":"1-0-0"},"type":"object"}""",
        superKey
      ),
      Expected(
        409,
        JsonCT,
        Body.Satisfies(
          "a clash with both existing schemas",
          _.hcursor
            .get[String]("message")
            .exists(m =>
              m.startsWith("{vendor}_{name} clash between iglu:com.acme/clash-test/jsonschema/1-0-0 and [") &&
                m.contains("(vendor=com.acme.clash, name=test)") && m.contains("(vendor=com.acme, name=clash_test)")
            )
        )
      )
    ),
    Case(
      "s46",
      "POST a schema with an invalid self.name",
      post(uri"/api/schemas", badName, superKey),
      Expected(400, JsonCT, Body.Satisfies("a lint report on /self/name", reportsBadName))
    ),
    Case(
      "s47",
      "POST a schema to the legacy validation route",
      post(uri"/api/schemas/validate/com.acme/new/jsonschema/1-0-0", newSchema, superKey),
      Expected.json(200, newSchemaReport)
    ),
    Case(
      "s48",
      "The Accept header is ignored",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0-0", ("Accept" -> "text/html")),
      Expected.json(200, event100)
    ),
    Case(
      "s49",
      "The first of repeated apikey headers wins, when it is unknown",
      get(
        uri"/api/schemas/com.acme/secret/jsonschema/1-0-0",
        apikey(SpecHelpers.nonExistentKey),
        apikey(SpecHelpers.readKeyAcme)
      ),
      Expected.message(404, "The schema is not found")
    ),
    Case(
      "s50",
      "The first of repeated apikey headers wins, when it can read",
      get(
        uri"/api/schemas/com.acme/secret/jsonschema/1-0-0",
        apikey(SpecHelpers.readKeyAcme),
        apikey(SpecHelpers.nonExistentKey)
      ),
      Expected.json(200, secret100)
    ),
    Case(
      "s51",
      "The apikey header name is case-insensitive",
      get(uri"/api/schemas/com.acme/secret/jsonschema/1-0-0", ("Apikey" -> SpecHelpers.readKeyAcme.toString)),
      Expected.json(200, secret100)
    ),
    Case(
      "s52",
      "OPTIONS on a schema without CORS headers",
      Request[IO](Method.OPTIONS, uri"/api/schemas/com.acme/event/jsonschema/1-0-0"),
      Expected.message(400, "Invalid number format: '1-0-0'")
    ),
    Case(
      "s53",
      "GET with an extra path segment reports the model route's integer parse error",
      get(uri"/api/schemas/com.acme/event/jsonschema/1-0-0/extra"),
      Expected.message(400, "Invalid number format: '1-0-0'")
    ),
    Case(
      "s54",
      "PATCH with an invalid SchemaVer",
      Request[IO](Method.PATCH, uri"/api/schemas/com.acme/event/jsonschema/not-a-ver"),
      Expected.message(400, "Cannot parse version part 'not-a-ver' as SchemaVer, INVALID_SCHEMAVER")
    ),
    Case(
      "s55",
      "HEAD with an invalid SchemaVer",
      Request[IO](Method.HEAD, uri"/api/schemas/com.acme/event/jsonschema/not-a-ver"),
      Expected.message(400, "Cannot parse version part 'not-a-ver' as SchemaVer, INVALID_SCHEMAVER")
    ),
    Case(
      "s56",
      "POST by model reports the invalid SchemaVer rather than a 405",
      Request[IO](Method.POST, uri"/api/schemas/com.acme/event/jsonschema/1"),
      Expected.message(400, "Cannot parse version part '1' as SchemaVer, INVALID_SCHEMAVER")
    )
  )

  val draft = put(uri"/api/drafts/com.acme/d/jsonschema/1", """{"type":"object"}""", superKey)

  val draftCases = List(
    Case(
      "d1",
      "GET a missing draft",
      get(uri"/api/drafts/com.acme/d/jsonschema/1", superKey),
      Expected.message(404, "The schema is not found")
    ),
    Case(
      "d2",
      "GET a draft with a non-integer version",
      get(uri"/api/drafts/com.acme/d/jsonschema/x", superKey),
      Expected.message(400, "x is not an integer")
    ),
    Case(
      "d3",
      "PUT a draft with an invalid isPublic",
      put(uri"/api/drafts/com.acme/d/jsonschema/1?isPublic=maybe", """{"type":"object"}""", superKey),
      Expected.message(400, "Invalid boolean format: 'maybe'")
    ),
    Case(
      "d4",
      "GET a draft created by PUT",
      get(uri"/api/drafts/com.acme/d/jsonschema/1", superKey),
      Expected(
        200,
        JsonCT,
        Body.Json(
          json"""{"schemaMap":{"vendor":"com.acme","name":"d","format":"jsonschema","version":1},"metadata":{"isPublic":false},"body":{"type":"object"}}""",
          withoutTimestamps
        )
      ),
      setup = List(draft)
    ),
    Case("d5", "PUT a draft", draft, Expected(501, TextCT, Body.Text("TODO")))
  )

  val eventInstance = """{"schema":"iglu:com.acme/event/jsonschema/1-0-0","data":{}}"""

  val validationCases = List(
    Case(
      "v1",
      "POST a schema to validate with an unknown format",
      post(uri"/api/validation/validate/schema/foo", newSchema),
      Expected.message(400, "Unknown schema format: 'foo'")
    ),
    Case(
      "v2",
      "POST an instance of an unknown schema to validate",
      post(uri"/api/validation/validate/instance", eventInstance),
      Expected.message(404, "The schema is not found")
    ),
    Case(
      "v3",
      "GET on the validation route",
      get(uri"/api/validation/validate/instance"),
      Expected.message(405, "GET not allowed. Defined methods: POST\n")
    ),
    Case(
      "v4",
      "POST a schema to validate",
      post(uri"/api/validation/validate/schema/jsonschema", newSchema, superKey),
      Expected.json(200, newSchemaReport)
    ),
    Case(
      "v5",
      "POST a schema with an invalid self.name to validate",
      post(uri"/api/validation/validate/schema/jsonschema", badName, superKey),
      Expected(200, JsonCT, Body.Satisfies("a lint report on /self/name", reportsBadName))
    ),
    Case(
      "v6",
      "POST a schema to validate with an unknown apikey",
      post(uri"/api/validation/validate/schema/jsonschema", newSchema, apikey(SpecHelpers.nonExistentKey)),
      Expected.message(403, "")
    ),
    Case(
      "v7",
      "POST a schema to validate without apikey",
      post(uri"/api/validation/validate/schema/jsonschema", newSchema),
      Expected.message(403, "")
    ),
    Case(
      "v8",
      "POST a valid instance to validate",
      post(uri"/api/validation/validate/instance", eventInstance, superKey),
      Expected.message(200, "Instance is valid iglu:com.acme/event/jsonschema/1-0-0")
    ),
    Case(
      "v9",
      "POST an invalid instance to validate",
      post(
        uri"/api/validation/validate/instance",
        """{"schema":"iglu:com.acme/secret/jsonschema/1-0-0","data":{}}""",
        superKey
      ),
      Expected.json(
        200,
        json"""{"message":"The data for a field instance is invalid against its schema","report":[{"message":"$$.password: is missing but it is required","path":"$$","keyword":"required","targets":["password"]}]}"""
      )
    )
  )

  val authCases = List(
    Case(
      "a1",
      "POST keygen with the super key",
      post(uri"/api/auth/keygen", """{"vendorPrefix":"com.acme"}""", superKey),
      Expected(
        200,
        JsonCT,
        Body.Satisfies(
          "read and write UUIDs",
          j =>
            j.asObject.map(_.keys.toSet).contains(Set("read", "write")) &&
              j.hcursor.get[UUID]("read").isRight && j.hcursor.get[UUID]("write").isRight
        )
      )
    ),
    Case(
      "a2",
      "DELETE keygen with a malformed key",
      delete(uri"/api/auth/keygen?key=not-a-uuid", superKey),
      Expected.message(400, "Invalid uuid format: not-a-uuid")
    ),
    Case(
      "a3",
      "DELETE keygen without key",
      delete(uri"/api/auth/keygen", superKey),
      Expected.message(400, "Missing query param: key")
    ),
    Case(
      "a4",
      "DELETE keygen with an unknown key",
      delete(uri"/api/auth/keygen?key=e8f1161f-5fbc-46df-b0ea-063f025e20c9", superKey),
      Expected.message(200, "Keys have been deleted")
    ),
    Case(
      "a5",
      "POST keygen without apikey",
      post(uri"/api/auth/keygen", """{"vendorPrefix":"com.acme"}"""),
      Expected.message(403, "Not sufficient privileges to create keys")
    ),
    Case(
      "a6",
      "DELETE keygen with a key parameter without value",
      delete(uri"/api/auth/keygen?key", superKey),
      Expected.message(400, "Value of query parameter 'key' missing")
    ),
    Case(
      "a7",
      "DELETE keygen with an empty key",
      delete(uri"/api/auth/keygen?key=", superKey),
      Expected.message(400, "Invalid uuid format: ")
    ),
    Case(
      "a8",
      "DELETE keygen with an existing key",
      delete(Uri.unsafeFromString(s"/api/auth/keygen?key=${SpecHelpers.readKeyAcme}"), superKey),
      Expected.message(200, "Keys have been deleted")
    ),
    Case(
      "a9",
      "POST keygen with a form-encoded body",
      Request[IO](Method.POST, uri"/api/auth/keygen")
        .withEntity(UrlForm("vendorPrefix" -> "com.acme"))
        .withHeaders(superKey),
      Expected.message(400, "The request body was malformed.")
    )
  )

  val metaCases = List(
    Case("m1", "GET health", get(uri"/api/meta/health"), Expected(200, TextCT, Body.Text("OK"))),
    Case("m2", "GET database health", get(uri"/api/meta/health/db"), Expected(200, TextCT, Body.Text("OK"))),
    Case(
      "m3",
      "GET health asking for JSON",
      get(uri"/api/meta/health", ("Accept" -> "application/json")),
      Expected(200, TextCT, Body.Text("OK"))
    ),
    Case(
      "m4",
      "GET server info with the super key",
      get(uri"/api/meta/server", superKey),
      Expected.json(
        200,
        json"""{"version":${BuildInfo.version},"authInfo":{"vendor":"","schema":"CREATE_VENDOR","key":["CREATE","DELETE"]},"database":"inmemory","schemaCount":5,"debug":false,"patchesAllowed":false}"""
      )
    ),
    Case(
      "m5",
      "GET server info without apikey",
      get(uri"/api/meta/server"),
      Expected.json(
        200,
        json"""{"version":${BuildInfo.version},"authInfo":{"vendor":"","schema":null,"key":[]},"database":"inmemory","schemaCount":0,"debug":false,"patchesAllowed":false}"""
      )
    )
  )

  val otherCases = List(
    Case("o1", "GET an unknown API path", get(uri"/api/unknown"), Expected.message(404, "The endpoint does not exist")),
    Case("o2", "GET the root", get(uri"/"), Expected.message(404, "The endpoint does not exist")),
    Case(
      "o3",
      "GET the Swagger UI",
      get(uri"/static/swagger-ui/index.html"),
      Expected(200, Some("text/html"), Body.StartsWith("<!-- HTML for static distribution bundle build -->"))
    ),
    Case(
      "o4",
      "GET a swagger.json",
      get(uri"/api/schemas/swagger.json"),
      Expected(
        200,
        JsonCT,
        Body.Satisfies("an OpenAPI 3.0 document", _.hcursor.get[String]("openapi").exists(_.startsWith("3.0.")))
      )
    ),
    Case(
      "o5",
      "GET debug when debug is off",
      get(uri"/api/debug", superKey),
      Expected.message(404, "The endpoint does not exist")
    ),
    Case(
      "o6",
      "GET debug when debug is on",
      get(uri"/api/debug", superKey),
      Expected(200, TextCT, Body.StartsWith("State(")),
      debug = true
    ),
    Case(
      "o7",
      "A CORS preflight request reflects any origin, without credentials",
      Request[IO](Method.OPTIONS, uri"/api/meta/health")
        .withHeaders(("Origin" -> "https://any.example"), ("Access-Control-Request-Method" -> "GET")),
      Expected(
        200,
        None,
        Body.Text(""),
        Map("Access-Control-Allow-Origin" -> "https://any.example"),
        Set("Access-Control-Allow-Credentials")
      )
    )
  )

  val event100Uri = uri"/api/schemas/com.acme/event/jsonschema/1-0-0"
  val updateEvent =
    put(event100Uri.withQueryParam("isPublic", "true"), """{"type":"object","properties":{"updated":{}}}""", superKey)
  val updated =
    json"""{"message":"Schema updated","updated":true,"location":"iglu:com.acme/event/jsonschema/1-0-0","status":200}"""

  /** Same app with `patchesAllowed = true`: DELETE, overwrites, and reads after a write */
  val patchCases = List(
    Case(
      "p1",
      "DELETE with the super key",
      delete(event100Uri, superKey),
      Expected.message(200, "Schema deleted"),
      patchesAllowed = true
    ),
    Case(
      "p2",
      "GET a schema read before it was deleted",
      get(event100Uri),
      Expected.message(404, "The schema is not found"),
      setup = List(get(event100Uri), delete(event100Uri, superKey)),
      patchesAllowed = true
    ),
    Case(
      "p3",
      "DELETE without the super key",
      delete(event100Uri, apikey(SpecHelpers.readKeyAcme)),
      Expected.message(401, "Not enough permissions"),
      patchesAllowed = true
    ),
    Case("p4", "PUT over an existing schema", updateEvent, Expected.json(200, updated), patchesAllowed = true),
    Case(
      "p5",
      "GET a schema read before it was updated",
      get(event100Uri),
      Expected.json(
        200,
        json"""{"$$schema":$SelfDesc,"self":{"vendor":"com.acme","name":"event","format":"jsonschema","version":"1-0-0"},"type":"object","properties":{"updated":{}}}"""
      ),
      setup = List(get(event100Uri), updateEvent),
      patchesAllowed = true
    ),
    Case(
      "p6",
      "POST over an existing schema",
      post(uri"/api/schemas?isPublic=true", newSchema, superKey),
      Expected.json(
        200,
        json"""{"message":"Schema updated","updated":true,"location":"iglu:com.acme/new/jsonschema/1-0-0","status":200}"""
      ),
      setup = List(post(uri"/api/schemas?isPublic=true", newSchema, superKey)),
      patchesAllowed = true
    ),
    Case(
      "p7",
      "PUT over an existing public schema without isPublic",
      put(event100Uri, """{"type":"object"}""", superKey),
      Expected
        .message(409, "Inconsistent schema availability. Cannot add private schema, previous versions are public"),
      patchesAllowed = true
    ),
    Case(
      "p8",
      "GET server info",
      get(uri"/api/meta/server"),
      Expected.json(
        200,
        json"""{"version":${BuildInfo.version},"authInfo":{"vendor":"","schema":null,"key":[]},"database":"inmemory","schemaCount":0,"debug":false,"patchesAllowed":true}"""
      ),
      patchesAllowed = true
    ),
    Case(
      "p9",
      "GET all schemas read before a DELETE",
      get(uri"/api/schemas"),
      Expected.json(
        200,
        json"""["iglu:com.acme/event/jsonschema/1-0-1","iglu:com.acme.clash/test/jsonschema/1-0-0","iglu:com.acme/clash_test/jsonschema/1-0-0"]"""
      ),
      setup = List(get(uri"/api/schemas"), delete(event100Uri, superKey)),
      patchesAllowed = true
    )
  )

  val keygenBody = """{"vendorPrefix":"com.acme"}"""
  val keyPair = Body.Satisfies(
    "read and write UUIDs",
    j => j.hcursor.get[UUID]("read").isRight && j.hcursor.get[UUID]("write").isRight
  )

  /** Bodies are sent as text/plain elsewhere. Snowman sends none, the Console sends application/json */
  val contentTypeCases = List(
    Case(
      "n1",
      "POST a schema without Content-Type",
      withoutContentType(post(uri"/api/schemas?isPublic=true", newSchema, superKey)),
      Expected.json(201, created)
    ),
    Case(
      "n2",
      "PUT a schema without Content-Type",
      withoutContentType(
        put(uri"/api/schemas/com.acme/new/jsonschema/1-0-0?isPublic=true", """{"type":"object"}""", superKey)
      ),
      Expected.json(201, created)
    ),
    Case(
      "n3",
      "POST a schema to the legacy validation route without Content-Type",
      withoutContentType(post(uri"/api/schemas/validate/com.acme/new/jsonschema/1-0-0", newSchema, superKey)),
      Expected.json(200, newSchemaReport)
    ),
    Case(
      "n4",
      "POST a schema to validate without Content-Type",
      withoutContentType(post(uri"/api/validation/validate/schema/jsonschema", newSchema, superKey)),
      Expected.json(200, newSchemaReport)
    ),
    Case(
      "n5",
      "POST an instance to validate without Content-Type",
      withoutContentType(post(uri"/api/validation/validate/instance", eventInstance, superKey)),
      Expected.message(200, "Instance is valid iglu:com.acme/event/jsonschema/1-0-0")
    ),
    Case(
      "n6",
      "POST keygen without Content-Type",
      withoutContentType(post(uri"/api/auth/keygen", keygenBody, superKey)),
      Expected(200, JsonCT, keyPair)
    ),
    Case(
      "n7",
      "PUT a draft without Content-Type",
      withoutContentType(draft),
      Expected(501, TextCT, Body.Text("TODO"))
    ),
    Case(
      "n8",
      "POST a schema as application/json",
      asJson(post(uri"/api/schemas?isPublic=true", newSchema, superKey)),
      Expected.json(201, created)
    ),
    Case(
      "n9",
      "PUT a schema as application/json",
      asJson(put(uri"/api/schemas/com.acme/new/jsonschema/1-0-0?isPublic=true", """{"type":"object"}""", superKey)),
      Expected.json(201, created)
    ),
    Case(
      "n10",
      "POST a schema to validate as application/json",
      asJson(post(uri"/api/validation/validate/schema/jsonschema", newSchema, superKey)),
      Expected.json(200, newSchemaReport)
    ),
    Case(
      "n11",
      "POST an instance to validate as application/json",
      asJson(post(uri"/api/validation/validate/instance", eventInstance, superKey)),
      Expected.message(200, "Instance is valid iglu:com.acme/event/jsonschema/1-0-0")
    ),
    Case(
      "n12",
      "POST keygen as application/json",
      asJson(post(uri"/api/auth/keygen", keygenBody, superKey)),
      Expected(200, JsonCT, keyPair)
    )
  )

  /**
    * A schema nested `depth` levels deep, under 100KB: the shape Snowman sends. `SpecHelpers.createDeepJsonSchema`
    * would exceed the payload size limit and get a 413.
    */
  def deepSchema(depth: Int): String = {
    val properties = SpecHelpers.createDeepJson(
      depth / 10,
      """{"l": {"properties":""",
      """, "type": "object", "description": "l"}}""",
      """{"deep": {"type": "string", "maxLength": 100, "description": "stop"}}"""
    )
    val nested = SpecHelpers.createDeepJson(depth, """{"1":""", "}", "\"deep\"")
    s"""{"$$schema":"$SelfDesc","type":"object","description":"Deep schema","self":{"vendor":"com.acme","name":"deep","format":"jsonschema","version":"1-0-0"},"properties":$properties,"x":$nested}"""
  }

  /**
    * Every route with a body checks its depth. The JSON parser can also have its own limit, which comes first: jawn
    * 1.7.0 rejects more than 4096 levels as malformed (400). Snowman sends 10000 and 5000 levels and expects the 422.
    */
  val depthCases = {
    val deepSDJ  = SpecHelpers.createDeepSDJ(10000)
    val veryDeep = deepSchema(5000)
    val invalid  = Expected.message(422, "The request body was invalid.")
    def veryDeepCase(id: String, what: String, request: Request[IO]): Case =
      Case(id, s"$what nested deeper than 4096 levels", request, invalid)
    List(
      veryDeepCase("j1", "An instance to validate", post(uri"/api/validation/validate/instance", deepSDJ, superKey)),
      veryDeepCase("j2", "A keygen body", post(uri"/api/auth/keygen", deepSDJ, superKey)),
      veryDeepCase(
        "j3",
        "A schema to validate",
        post(uri"/api/validation/validate/schema/jsonschema", veryDeep, superKey)
      ),
      veryDeepCase(
        "j4",
        "A schema to the legacy validation route",
        post(uri"/api/schemas/validate/com.acme/deep/jsonschema/1-0-0", veryDeep, superKey)
      ),
      veryDeepCase("j5", "A POSTed schema", post(uri"/api/schemas", veryDeep, superKey)),
      veryDeepCase("j6", "A PUT schema", put(uri"/api/schemas/com.acme/deep/jsonschema/1-0-0", veryDeep, superKey)),
      veryDeepCase("j7", "A draft", put(uri"/api/drafts/com.acme/deep/jsonschema/1", veryDeep, superKey)),
      Case(
        "j8",
        s"An instance to validate deeper than maxJsonDepth ($MaxJsonDepth) but within 4096 levels",
        post(uri"/api/validation/validate/instance", SpecHelpers.createDeepSDJ(100), superKey),
        invalid
      )
    )
  }

  val allCases =
    schemaCases ++ draftCases ++ validationCases ++ authCases ++ metaCases ++ otherCases ++ patchCases ++
      contentTypeCases ++ depthCases
}
