package com.snowplowanalytics.iglu.server.service

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}
import com.snowplowanalytics.iglu.server.model.Schema.SupersedingInfo
import com.snowplowanalytics.iglu.server.model.SchemaSpec.testSchema
import com.snowplowanalytics.iglu.server.{SpecHelpers, Webhook}
import com.snowplowanalytics.iglu.server.SpecHelpers.SchemaKeyUri
import io.circe._
import org.http4s.circe._
import org.http4s.rho.swagger.syntax.io.createRhoMiddleware
import org.http4s.{Header, Headers, Method, Request, Response}

trait SupersedingLogicSpecBase extends org.specs2.Specification with StorageAgnosticSpec {
  private def sendRequests(requests: List[Request[IO]]): IO[(List[Response[IO]], Unit)] =
    sendRequestsGetState(storage =>
      SchemaService.asRoutes(patchesAllowed = true, Webhook.WebhookClient(List(), client))(
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
      supersedingInfo = (supersededBy, supersedes) match {
        case (_, head :: tail) =>
          Some(SupersedingInfo.Supersedes(NonEmptyList(tripletToVersion(head), tail.map(tripletToVersion))))
        case (Some(version), _) =>
          Some(SupersedingInfo.SupersededBy(tripletToVersion(version)))
        case _ =>
          None
      }
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
    } yield json.drop(schemas.length)
    result.unsafeRunSync()
  }

  private def validate(input: List[S], expected: List[S]) = {
    // obviously
    val logicWorks = sendReceive(input.map(_.encode)) must beEqualTo(expected.map(_.encode._1))

    // the order of `supersedes` declarations should not matter
    // let’s try to patch them in all possible permutations
    val distinctSchemas    = input.map(_.copy(supersedes = List.empty)).distinct
    val supersedingSchemas = input.filter(_.supersedes.nonEmpty)
    val orderDoesNotMatter = supersedingSchemas
      .permutations
      .toList
      .map { shuffled =>
        sendReceive((distinctSchemas ::: shuffled).map(_.encode)) must beEqualTo(expected.map(_.encode._1))
      }
      .reduce(_.and(_))

    // if we load the result into another Iglu Server,
    // (which is what happens when promoting schemas from DEV to PROD)
    // we should get exactly the same
    // (note that this might be trivially true for the current implementation,
    // but does not hurt to check)
    val promotionWorks = sendReceive(expected.map(_.encode)) must beEqualTo(expected.map(_.encode._1))

    logicWorks.and(orderDoesNotMatter).and(promotionWorks)
  }

  def is = sequential ^ s2"""
  Basics
    Schema version can be superseded by a newer version $e1
    Schema version can be superseded via patching $e2
    Schema version can’t be superseded by an older version $e3
    Schema version can supersede multiple versions $e4
    Schema version can supersede multiple versions via patching $e5
  Chaining
    If B<-A and C<-A, then C<-A $e6
    If C<-A and B<-A, then C<-A $e7
    If B<-A and C<-B, then C<-A and C<-B $e8
    If C<-A and C<-B and D<-C, then D<-A and D<-B and D<-C $e9
    If B<-A and D<-C and C<-B, then D<-A and D<-B and D<-C $e10
    If C<-B and D<-A and B<-A, then D<-A and C<-B $e11
    A long chain like E<-D<-C<-B<-A works $e12
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
      S((1, 0, 0), supersededBy = Some((1, 0, 1))),
      S((1, 0, 1), supersedes = List((1, 0, 0)))
    )

    validate(input, expected)
  }

  def e3 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((1, 0, 2)),
      S((1, 0, 1), supersedes = List((1, 0, 2)))
    )

    val expected = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((1, 0, 2))
    )

    validate(input, expected)
  }

  def e5 = {
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

  def e4 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((1, 0, 2), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersedes = List((1, 0, 1)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 2))),
      S((1, 0, 1), supersededBy = Some((1, 0, 2))),
      S((1, 0, 2), supersedes = List((1, 0, 0), (1, 0, 1)))
    )

    validate(input, expected)
  }

  def e6 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersedes = List((1, 0, 0)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 2))),
      S((1, 0, 1)),
      S((1, 0, 2), supersedes = List((1, 0, 0)))
    )

    validate(input, expected)
  }

  def e7 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((1, 0, 2), supersedes = List((1, 0, 0))),
      S((1, 0, 1), supersedes = List((1, 0, 0)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 2))),
      S((1, 0, 1)),
      S((1, 0, 2), supersedes = List((1, 0, 0)))
    )

    validate(input, expected)
  }

  def e8 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersedes = List((1, 0, 1)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 2))),
      S((1, 0, 1), supersededBy = Some((1, 0, 2))),
      S((1, 0, 2), supersedes = List((1, 0, 0), (1, 0, 1)))
    )

    validate(input, expected)
  }

  def e9 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((1, 0, 2), supersedes = List((1, 0, 0), (1, 0, 1))),
      S((1, 0, 3), supersedes = List((1, 0, 2)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 3))),
      S((1, 0, 1), supersededBy = Some((1, 0, 3))),
      S((1, 0, 2), supersededBy = Some((1, 0, 3))),
      S((1, 0, 3), supersedes = List((1, 0, 0), (1, 0, 1), (1, 0, 2)))
    )

    validate(input, expected)
  }

  def e10 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1), supersedes = List((1, 0, 0))),
      S((1, 0, 2)),
      S((1, 0, 3), supersedes = List((1, 0, 2))),
      S((1, 0, 2), supersedes = List((1, 0, 1)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 3))),
      S((1, 0, 1), supersededBy = Some((1, 0, 3))),
      S((1, 0, 2), supersededBy = Some((1, 0, 3))),
      S((1, 0, 3), supersedes = List((1, 0, 0), (1, 0, 1), (1, 0, 2)))
    )

    validate(input, expected)
  }

  def e11 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1)),
      S((1, 0, 2), supersedes = List((1, 0, 1))),
      S((1, 0, 3), supersedes = List((1, 0, 0))),
      S((1, 0, 1), supersedes = List((1, 0, 0)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 3))),
      S((1, 0, 1), supersededBy = Some((1, 0, 2))),
      S((1, 0, 2), supersedes = List((1, 0, 1))),
      S((1, 0, 3), supersedes = List((1, 0, 0)))
    )

    validate(input, expected)
  }

  def e12 = {
    val input = List(
      S((1, 0, 0)),
      S((1, 0, 1), supersedes = List((1, 0, 0))),
      S((1, 0, 2), supersedes = List((1, 0, 1))),
      S((1, 0, 3), supersedes = List((1, 0, 2))),
      S((1, 0, 4), supersedes = List((1, 0, 3)))
    )

    val expected = List(
      S((1, 0, 0), supersededBy = Some((1, 0, 4))),
      S((1, 0, 1), supersededBy = Some((1, 0, 4))),
      S((1, 0, 2), supersededBy = Some((1, 0, 4))),
      S((1, 0, 3), supersededBy = Some((1, 0, 4))),
      S((1, 0, 4), supersedes = List((1, 0, 0), (1, 0, 1), (1, 0, 2), (1, 0, 3)))
    )

    validate(input, expected)
  }
}

class SupersedingLogicSpec extends SupersedingLogicSpecBase with InMemoryStorageSpec

class SupersedingLogicSpecPostgres extends SupersedingLogicSpecBase with PostgresStorageSpec
