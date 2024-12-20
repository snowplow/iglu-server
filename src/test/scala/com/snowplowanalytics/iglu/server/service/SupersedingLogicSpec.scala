/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.service

import cats.effect.IO
import cats.implicits._
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}
import com.snowplowanalytics.iglu.server.model.Schema.SupersedingInfo
import com.snowplowanalytics.iglu.server.model.SchemaSpec.testSchema
import com.snowplowanalytics.iglu.server.{SpecHelpers, Webhook}
import com.snowplowanalytics.iglu.server.SpecHelpers.SchemaKeyUri
import io.circe._
import io.circe.literal._
import org.http4s.circe._
import org.http4s.rho.swagger.syntax.io.createRhoMiddleware
import org.http4s.{Header, Headers, Method, Request, Response}

trait SupersedingLogicSpecBase extends org.specs2.Specification with StorageAgnosticSpec {
  private def sendRequests(requests: List[Request[IO]]): IO[(List[Response[IO]], Unit)] =
    sendRequestsGetState(storage =>
      SchemaService.asRoutes(patchesAllowed = true, Webhook.WebhookClient(List(), client), 20)(
        storage,
        None,
        SpecHelpers.ctx,
        createRhoMiddleware()
      )
    )(_ => IO.unit)(requests)

  case class S(
    version: (Int, Int, Int),
    supersededBy: Option[(Int, Int, Int)] = Option.empty,
    supersedes: List[(Int, Int, Int)] = List.empty
  ) {
    def encode = testSchema(
      tripletToVersion(version),
      SupersedingInfo(supersededBy.map(tripletToVersion), supersedes.map(tripletToVersion))
    )
  }

  private def tripletToVersion(version: (Int, Int, Int)) =
    SchemaVer.Full(version._1, version._2, version._3)

  private def sendReceive(schemas: List[(Json, SchemaKey)]): List[Json] = {
    val putRequests = schemas.map {
      case (body, key) =>
        Request[IO](Method.PUT, key.uri)
          .withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
          .withEntity(body)
    }
    val getRequests = schemas.map(_._2).distinct.map { key =>
      Request[IO](Method.GET, key.uri).withHeaders(Headers.of(Header("apikey", SpecHelpers.superKey.toString)))
    }
    val result = for {
      responses <- sendRequests(putRequests ::: getRequests)
      json      <- responses._1.map(_.as[Json]).sequence
    } yield json.drop(schemas.length).filterNot(_ == json"""{ "message": "The schema is not found" }""")
    result.unsafeRunSync()
  }

  private def validate(input: List[S], expected: List[S]) = {
    // obviously
    val logicWorks = sendReceive(input.map(_.encode)) must beEqualTo(expected.map(_.encode._1))

    // if we load the result into another Iglu Server,
    // (which is what happens when promoting schemas from DEV to PROD)
    // we should get exactly the same
    // (note that this might be trivially true for the current implementation,
    // but does not hurt to check)
    val promotionWorks = sendReceive(expected.map(_.encode)) must beEqualTo(expected.map(_.encode._1))

    logicWorks.and(promotionWorks)
  }

  def is = sequential ^ s2"""
  Basics
    Schema version can supersede an older version $e1
    Schema version can’t be superseded via patching $e2
    Schema version can’t supersede a newer version $e3a
    Schema version can’t supersede a nonexistent version $e3b
    Schema version can supersede multiple versions $e4
  Chaining
    If B<-A and C<-A, then C<-A $e5a
    If C<-A and B<-A, then C<-A $e5b
    If B<-A and C<-B, then C<-A and C<-B $e6
    If C<-A and C<-B and D<-C, then D<-A and D<-B and D<-C $e7
    A long chain like E<-D<-C<-B<-A works $e8
  """

  def e1 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1), supersedes = List((1, 0, 0)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 1))),
      S((1, 0, 1), supersedes = List((1, 0, 0)))
    )

    validate(input, expected)
  }

  def e2 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((1, 0, 1), supersedes = List((1, 0, 0)))
    )

    val expected = List(
      S((1, 0, 0)),
      S((1, 0, 1))
    )

    validate(input, expected)
  }

  def e3a = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((2, 0, 0)),
      S((1, 0, 2), supersedes = List((2, 0, 0)))
    )

    val expected = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((2, 0, 0))
    )

    validate(input, expected)
  }

  def e3b = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((2, 0, 0), supersedes = List((1, 0, 2)))
    )

    val expected = List(
      S((1, 0, 0)),
      S((1, 0, 1))
    )

    validate(input, expected)
  }

  def e4 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((1, 0, 2), supersedes = List((1, 0, 0), (1, 0, 1)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 2))),
      S((1, 0, 1), supersededBy = Some((1, 0, 2))),
      S((1, 0, 2), supersedes = List((1, 0, 0), (1, 0, 1)))
    )

    validate(input, expected)
  }

  def e5a = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersedes = List((1, 0, 0)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 2))),
      S((1, 0, 1), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersedes = List((1, 0, 0)))
    )

    validate(input, expected)
  }

  def e5b = {
    val input = List(
      S((1, 0, 0)),
      S((2, 0, 0), supersedes = List((1, 0, 0))),
      S((1, 0, 1), supersedes = List((1, 0, 0)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((2, 0, 0))),
      S((2, 0, 0), supersedes = List((1, 0, 0))),
      S((1, 0, 1), supersedes = List((1, 0, 0)))
    )

    validate(input, expected)
  }

  def e6 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersedes = List((1, 0, 1)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 2))),
      S((1, 0, 1), supersededBy = Some((1, 0, 2)), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersedes = List((1, 0, 1)))
    )

    validate(input, expected)
  }

  def e7 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((1, 0, 2), supersedes = List((1, 0, 0), (1, 0, 1))),
      S((1, 0, 3), supersedes = List((1, 0, 2)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 3))),
      S((1, 0, 1), supersededBy = Some((1, 0, 3))),
      S((1, 0, 2), supersededBy = Some((1, 0, 3)), supersedes = List((1, 0, 0), (1, 0, 1))),
      S((1, 0, 3), supersedes = List((1, 0, 2)))
    )

    validate(input, expected)
  }

  def e8 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersedes = List((1, 0, 1))),
      S((1, 0, 3), supersedes = List((1, 0, 2))),
      S((1, 0, 4), supersedes = List((1, 0, 3)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 4))),
      S((1, 0, 1), supersededBy = Some((1, 0, 4)), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersededBy = Some((1, 0, 4)), supersedes = List((1, 0, 1))),
      S((1, 0, 3), supersededBy = Some((1, 0, 4)), supersedes = List((1, 0, 2))),
      S((1, 0, 4), supersedes = List((1, 0, 3)))
    )

    validate(input, expected)
  }
}

class SupersedingLogicSpec extends SupersedingLogicSpecBase with InMemoryStorageSpec

class SupersedingLogicSpecPostgres extends SupersedingLogicSpecBase with PostgresStorageSpec
