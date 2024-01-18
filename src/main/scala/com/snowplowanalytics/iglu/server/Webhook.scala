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

import cats.effect.BracketThrow
import cats.implicits._

import io.circe.{Encoder, Json}
import io.circe.generic.semiauto._

import org.http4s.{Request, Response, Status, Uri}
import org.http4s.client.Client
import org.http4s.circe._
import org.http4s.Method
import com.snowplowanalytics.iglu.core.SchemaKey

sealed trait Webhook

object Webhook {

  case class SchemaPublished(
    uri: Uri,
    vendorPrefixes: Option[List[String]],
    usePost: Boolean
  ) extends Webhook

  case class WebhookClient[F[_]](webhooks: List[Webhook], httpClient: Client[F]) {
    def schemaPublished(schemaKey: SchemaKey, updated: Boolean)(
      implicit F: BracketThrow[F]
    ): F[List[Either[String, Unit]]] =
      webhooks.traverse {
        case SchemaPublished(uri, prefixes, usePost)
            if prefixes.isEmpty || prefixes.getOrElse(List()).exists(schemaKey.vendor.startsWith(_)) =>
          val event = SchemaPublishedEvent(schemaKey, updated)
          val req = Request[F]()
            .withUri(uri)
            .withMethod(if (usePost) Method.POST else Method.GET)
            .withBodyStream(Utils.toBytes(event))
          httpClient.run(req).use { res: Response[F] =>
            res.status match {
              case Status(code) if code < 200 || code > 299 => F.pure(code.toString.asLeft[Unit])
              case _                                        => F.pure(().asRight[String])
            }
          }
        case _ => F.pure(().asRight)
      }
  }

  case class SchemaPublishedEvent(schemaKey: SchemaKey, updated: Boolean)

  implicit val schemaPublishedEventEncoder: Encoder[SchemaPublishedEvent] =
    Encoder.instance { event =>
      Json.fromFields(
        List(
          "schemaKey" -> Json.fromString(event.schemaKey.toSchemaUri),
          "updated"   -> Json.fromBoolean(event.updated)
        )
      )
    }

  implicit val webhookEncoder: Encoder[Webhook] =
    deriveEncoder[Webhook]
}
