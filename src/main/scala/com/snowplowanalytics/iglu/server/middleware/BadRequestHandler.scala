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
            .map(s => Response(res.status).withBodyStream(s).withContentType(`Content-Type`(MediaType.application.json))
            )
        else Effect[G].pure(res)

      OptionT[G, Response[G]](http(req).value.flatMap(o => o.traverse(handle)))
    }
}
