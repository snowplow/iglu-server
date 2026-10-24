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

import cats.effect.Async
import cats.implicits._

import sttp.model.StatusCode
import sttp.tapir.{PublicEndpoint, endpoint, stringBody}

import com.snowplowanalytics.iglu.server.http.Api
import com.snowplowanalytics.iglu.server.http.Endpoints._
import com.snowplowanalytics.iglu.server.storage.{InMemory, Storage}

/** Service showing whole in-memory state. Use for development only */
object DebugService {

  def stateEndpoint: PublicEndpoint[Unit, Failure, String, Any] =
    endpoint
      .get
      .in(root)
      .errorOut(failureOutput(StatusCode.NotImplemented))
      .out(stringBody)
      .summary("Show internal state")

  def api[F[_]: Async](db: Storage[F]): Api[F] =
    Api(
      List(
        Api.withPermission(stateEndpoint) { _ => _ =>
          db match {
            case InMemory(ref) => ref.get.map(_.toString.asRight[Failure])
            case other         => Async[F].pure(Failure.text(StatusCode.NotImplemented, s"Cannot show $other").asLeft)
          }
        }
      )
    )
}
