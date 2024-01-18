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
import cats.effect.IO

import org.http4s._
import org.http4s.client.Client

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}
import com.snowplowanalytics.iglu.server.Webhook.WebhookClient

class WebhookSpec extends org.specs2.Specification {
  def is = s2"""
  Return Unit results for successful requests    $e1
  Return status code results for failed requests $e2
  """

  def e1 = {
    val response = WebhookSpec
      .webhookClient
      .schemaPublished(SchemaKey("com.acme", "event", "jsonschema", SchemaVer.Full(1, 0, 0)), true)
    response.unsafeRunSync() mustEqual List(().asRight, ().asRight, ().asRight)
  }

  def e2 = {
    val response = WebhookSpec
      .badWebhookClient
      .schemaPublished(SchemaKey("com.acme", "event", "jsonschema", SchemaVer.Full(1, 0, 0)), true)
    response.unsafeRunSync() mustEqual List("502".asLeft, "502".asLeft, "502".asLeft)
  }
}

object WebhookSpec {
  val webhooks = List(
    Webhook.SchemaPublished(Uri.uri("https://example.com/endpoint"), None, usePost = false),
    Webhook.SchemaPublished(
      Uri.uri("https://example2.com/endpoint"),
      Some(List("com", "org.acme", "org.snowplow")),
      usePost = false
    ),
    Webhook.SchemaPublished(Uri.uri("https://example3.com/endpoint"), None, usePost = true)
  )

  val client: Client[IO] = Client.fromHttpApp(HttpApp[IO](r => Response[IO]().withEntity(r.body).pure[IO]))
  val badClient: Client[IO] =
    Client.fromHttpApp(HttpApp[IO](r => Response[IO]().withStatus(Status.BadGateway).withEntity(r.body).pure[IO]))

  val webhookClient    = WebhookClient(webhooks, client)
  val badWebhookClient = WebhookClient(webhooks, badClient)
}
