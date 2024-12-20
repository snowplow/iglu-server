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

import cats.effect.{IO, Sync}
import cats.implicits._

import org.http4s.rho.{RhoMiddleware, RhoRoutes}
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.rho.swagger.syntax.{io => swaggerSyntax}

import com.snowplowanalytics.iglu.server.storage.{InMemory, Storage}

/** Service showing whole in-memory state. Use for development only */
class DebugService[F[_]: Sync](swagger: SwaggerSyntax[F], db: Storage[F]) extends RhoRoutes[F] {
  import swagger._

  "Show internal state" **
    GET |>> {
    db match {
      case InMemory(ref) =>
        for {
          db       <- ref.get
          response <- Ok(db.toString)
        } yield response
      case other => NotImplemented(s"Cannot show $other")
    }
  }
}

object DebugService {
  def asRoutes(db: Storage[IO], rhoMiddleware: RhoMiddleware[IO]) =
    new DebugService(swaggerSyntax, db).toRoutes(rhoMiddleware)
}
