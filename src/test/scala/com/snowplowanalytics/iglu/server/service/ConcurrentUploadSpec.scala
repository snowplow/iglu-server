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

import cats.implicits._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import io.circe.literal._
import org.http4s._
import org.http4s.implicits._
import com.snowplowanalytics.iglu.core.{SchemaMap, SchemaVer}

class ConcurrentUploadSpec extends org.specs2.Specification with StorageAgnosticSpec with PostgresStorageSpec {
  def is = sequential ^ s2"""
  Concurrent uploads of the same schema version
    create a single schema and reject the others when patches are disallowed $e1
    create a single schema and update it with the others when patches are allowed $e2
  """

  // Source.fromResource uses the context classloader of the current thread, which cannot see
  // iglu-core's meta-schema from pool threads under sbt's in-process test runner. Initialise it
  // here, on the test thread, before the concurrent requests validate schemas on pool threads.
  val metaSchema: String = com.snowplowanalytics.iglu.core.circe.MetaSchemas.JsonSchemaV4Text

  val Concurrency = 20
  val schemaMap   = SchemaMap("com.acme", "concurrent", "jsonschema", SchemaVer.Full(1, 0, 0))

  /** Upload distinct bodies for the same version in parallel, return the status counts and stored rows */
  def uploadConcurrently(patchesAllowed: Boolean): (Map[Status, Int], Int) = {
    val requests = (1 to Concurrency).toList.map { i =>
      val body = json"""{"type": "object", "description": ${s"upload $i"}}""".noSpaces
      Request[IO](Method.PUT, uri"/com.acme/concurrent/jsonschema/1-0-0")
        .withContentType(headers.`Content-Type`(MediaType.application.json))
        .withHeaders(Headers("apikey" -> SpecHelpers.superKey.toString))
        .withBodyStream(Stream.emits(body.getBytes).covary[IO])
    }

    storageResource
      .use { storage =>
        val service =
          routes(SchemaService.api[IO](patchesAllowed, Webhook.WebhookClient(List(), client), 20)(storage), storage)
        for {
          responses <- requests.parTraverse(request => service.run(request).value)
          schemas   <- storage.getSchemas
        } yield (
          responses.flatten.groupBy(_.status).map { case (status, rs) => status -> rs.size },
          schemas.count(_.schemaMap == schemaMap)
        )
      }
      .unsafeRunSync()
  }

  def e1 = {
    val (statuses, rows) = uploadConcurrently(patchesAllowed = false)
    (statuses must beEqualTo(Map(Status.Created -> 1, Status.Conflict -> (Concurrency - 1))))
      .and(rows must beEqualTo(1))
  }

  def e2 = {
    val (statuses, rows) = uploadConcurrently(patchesAllowed = true)
    (statuses must beEqualTo(Map(Status.Created -> 1, Status.Ok -> (Concurrency - 1)))).and(rows must beEqualTo(1))
  }
}
