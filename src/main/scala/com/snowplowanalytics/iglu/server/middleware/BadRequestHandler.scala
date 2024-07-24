/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.middleware

import cats.data.{Kleisli, OptionT}
import cats.effect.Effect
import cats.implicits._

import com.snowplowanalytics.iglu.server.Utils
import com.snowplowanalytics.iglu.server.model.IgluResponse

import io.circe.parser.parse

import org.http4s.{HttpRoutes, MediaType, Request, Response}
import org.http4s.headers.`Content-Type`

/** Wrap any non-JSON message into `{"message": original}` payload */
object BadRequestHandler {
  def apply[G[_]: Effect](http: HttpRoutes[G]): HttpRoutes[G] =
    Kleisli { req: Request[G] =>
      def handle(res: Response[G]): G[Response[G]] =
        if (res.status.code >= 400 && res.status.code < 500)
          res
            .bodyText
            .compile
            .foldMonoid
            .fproduct(parse)
            .map { case (b, e) => Utils.toBytes(e.fold(_ => IgluResponse.Message(b).asJson, identity)) }
            .map(s =>
              Response(res.status).withBodyStream(s).withContentType(`Content-Type`(MediaType.application.json))
            )
        else Effect[G].pure(res)

      OptionT[G, Response[G]](http(req).value.flatMap(o => o.traverse(handle)))
    }
}
