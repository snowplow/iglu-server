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
package service

import cats.effect.IO
import fs2.Stream
import org.http4s._
import org.http4s.rho.swagger.syntax.io.createRhoMiddleware

import java.util.UUID
import com.snowplowanalytics.iglu.server.storage.InMemory
import com.snowplowanalytics.iglu.server.SpecHelpers._

class AuthServiceSpec extends org.specs2.Specification with StorageAgnosticSpec with InMemoryStorageSpec {
  def getState(
    request: Request[IO],
    superApiKey: Option[UUID] = None,
    maxJsonDepth: Int = 20
  ): IO[(List[Response[IO]], InMemory.State)] =
    sendRequestsGetState[InMemory.State](storage =>
      AuthService.asRoutes(maxJsonDepth)(storage, superApiKey, SpecHelpers.ctx, createRhoMiddleware())
    )(storage => storage.asInstanceOf[InMemory[IO]].ref.get)(List(request))

  def is = s2"""
  /keygen generates read-only API key pair for super key in storage and JSON payload $e1
  /keygen generates read-only API key pair for super key in config file and JSON payload $e2
  /keygen does not work with deprecated form API $e3
  /keygen doesn't authorize without apikey header $e4
  /keygen doesn't authorize with unkown apikey in header $e5
  /keygen deletes key $e6
  /keygen rejects JSON body that exceeds maximum allowed JSON depth $e7
  """

  def e1 = {
    import model._

    val req = Request(
      Method.POST,
      Uri.uri("/keygen"),
      headers = Headers.of(Header("apikey", SpecHelpers.superKey.toString)),
      body = Stream.emits("""{"vendorPrefix": "me.chuwy"}""").evalMap(c => IO.pure(c.toByte))
    )

    val expected = Permission(
      Permission.Vendor(List("me", "chuwy"), true),
      Some(Permission.SchemaAction.Read),
      Set()
    )

    val response   = getState(req)
    val (_, state) = response.unsafeRunSync()
    state.permission must haveValues(expected)
  }

  def e2 = {
    import model._

    val superApiKey = UUID.fromString("a480e86f-3e74-4ebb-87cc-ac205f9ab69c")

    val req = Request(
      Method.POST,
      Uri.uri("/keygen"),
      headers = Headers.of(Header("apikey", superApiKey.toString)),
      body = Stream.emits("""{"vendorPrefix": "me.chuwy"}""").evalMap(c => IO.pure(c.toByte))
    )

    val expected = Permission(
      Permission.Vendor(List("me", "chuwy"), true),
      Some(Permission.SchemaAction.Read),
      Set()
    )

    val response   = getState(req, Some(superApiKey))
    val (_, state) = response.unsafeRunSync()
    state.permission must (haveValues(expected))
  }

  def e3 = {
    import model._

    val req = Request(
      Method.POST,
      Uri.uri("/keygen"),
      headers = Headers.of(Header("apikey", SpecHelpers.superKey.toString)),
      body = Stream.emits("""vendor_prefix=ru.chuwy""").evalMap(c => IO.pure(c.toByte))
    ).withContentType(headers.`Content-Type`(MediaType.application.`x-www-form-urlencoded`))

    val expected = Permission(
      Permission.Vendor(List("ru", "chuwy"), true),
      Some(Permission.SchemaAction.Read),
      Set()
    )

    val response   = getState(req)
    val (_, state) = response.unsafeRunSync()
    (state.permission must not).haveValues(expected)
  }

  def e4 = {
    val req =
      Request(
        Method.POST,
        Uri.uri("/keygen"),
        body = Stream.emits("""{"vendorPrefix": "me.chuwy"}""").evalMap(c => IO.pure(c.toByte))
      )

    val response           = getState(req)
    val (responses, state) = response.unsafeRunSync()
    val stateHaventChanged = state must beEqualTo(SpecHelpers.exampleState)
    val unauthorized       = responses.map(_.status) must beEqualTo(List(Status.Forbidden))

    stateHaventChanged.and(unauthorized)
  }

  def e5 = {

    val superApiKey = UUID.fromString("9d9515d3-decf-4733-b3de-60b73a6b2e2d")
    val unknownKey  = UUID.fromString("83e86bed-8793-4a8a-b4f1-5edfe23ffc15")

    val req =
      Request(
        Method.POST,
        Uri.uri("/keygen"),
        headers = Headers.of(Header("apikey", unknownKey.toString)),
        body = Stream.emits("""{"vendorPrefix": "me.chuwy"}""").evalMap(c => IO.pure(c.toByte))
      )

    val response           = getState(req, Some(superApiKey))
    val (responses, state) = response.unsafeRunSync()
    val stateHaventChanged = state must beEqualTo(SpecHelpers.exampleState)
    val unauthorized       = responses.map(_.status) must beEqualTo(List(Status.Forbidden))

    stateHaventChanged.and(unauthorized)
  }

  def e6 = {
    val req = Request[IO](
      Method.DELETE,
      Uri.uri("/keygen").withQueryParam("key", SpecHelpers.readKey.toString),
      headers = Headers.of(Header("apikey", SpecHelpers.superKey.toString))
    )

    val response           = getState(req)
    val (responses, state) = response.unsafeRunSync()
    val nokey              = (state.permission must not).haveKey(SpecHelpers.readKey)
    val deletedResponse    = responses.map(_.status) must beEqualTo(List(Status.Ok))

    nokey.and(deletedResponse)
  }

  def e7 = {
    val deepJsonSchema = createDeepJsonSchema(100000)
    val deepJsonArray  = createDeepJsonArray(1000000)
    val wrongApikey    = "c99ce0f9-cb5b-4b6f-88f3-2baed041be9b"

    def executeTest(body: String, apikey: String) = {
      val req = Request(
        Method.POST,
        Uri.uri("/keygen"),
        headers = Headers.of(Header("apikey", apikey)),
        body = toBytes(body)
      )

      val expected = List((422, "The request body was invalid."))

      val (resp, _) = getState(req).unsafeRunSync()
      val result    = resp.map(r => (r.status.code, r.bodyText.compile.foldMonoid.unsafeRunSync()))

      result must beEqualTo(expected)
    }

    executeTest(deepJsonSchema, SpecHelpers.superKey.toString)
    executeTest(deepJsonArray, wrongApikey)
    executeTest(deepJsonSchema, SpecHelpers.superKey.toString)
    executeTest(deepJsonArray, wrongApikey)
  }
}
