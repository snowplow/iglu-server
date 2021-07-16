/*
 * Copyright (c) 2019-2021 Snowplow Analytics Ltd. All rights reserved.
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

import cats.effect.IO
import fs2.Stream
import org.http4s._
import org.http4s.rho.swagger.syntax.io.createRhoMiddleware
import java.util.UUID

import com.snowplowanalytics.iglu.server.storage.InMemory

class AuthServiceSpec extends org.specs2.Specification {
  def is = s2"""
  /keygen generates read-only API key pair for super key in storage and JSON payload $e1
  /keygen generates read-only API key pair for super key in config file and JSON payload $e2
  /keygen does not work with deprecated form API $e3
  /keygen doesn't authorize without apikey header $e4
  /keygen doesn't authorize with unkown apikey in header $e5
  /keygen deletes key $e6
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

    val response   = AuthServiceSpec.state(req)
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

    val response   = AuthServiceSpec.state(req, Some(superApiKey))
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

    val response   = AuthServiceSpec.state(req)
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

    val response           = AuthServiceSpec.state(req)
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

    val response           = AuthServiceSpec.state(req, Some(superApiKey))
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

    val response           = AuthServiceSpec.state(req)
    val (responses, state) = response.unsafeRunSync()
    val nokey              = (state.permission must not).haveKey(SpecHelpers.readKey)
    val deletedResponse    = responses.map(_.status) must beEqualTo(List(Status.Ok))

    nokey.and(deletedResponse)
  }
}

object AuthServiceSpec {
  def state(req: Request[IO], superApiKey: Option[UUID] = None): IO[(List[Response[IO]], InMemory.State)] =
    SpecHelpers.state(storage => AuthService.asRoutes(storage, superApiKey, SpecHelpers.ctx, createRhoMiddleware()))(
      List(req)
    )
}
