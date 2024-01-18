/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.0
 * located at https://docs.snowplow.io/limited-use-license-1.0
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.service

import cats.effect.{Blocker, ContextShift, IO}

import org.http4s.{HttpRoutes, Request, Response, StaticFile}
import org.http4s.dsl.io._

import com.snowplowanalytics.iglu.server.generated.BuildInfo

object StaticService {
  private val localUi      = "/swagger-ui-dist"
  private val swaggerUiDir = s"/META-INF/resources/webjars/swagger-ui/${BuildInfo.SwaggerUI}"

  /**
    * Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
    * but its nice to keep it self contained
    */
  def routes(blocker: Blocker)(implicit cs: ContextShift[IO]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      // Swagger User Interface
      case req @ GET -> Root / "swagger-ui" / "index.html" => fetchResource(localUi + "/index.html", blocker, req)
      case req @ GET -> Root / "swagger-ui" / path         => fetchResource(swaggerUiDir + "/" + path, blocker, req)
    }

  def fetchResource(path: String, blocker: Blocker, req: Request[IO])(implicit cs: ContextShift[IO]): IO[Response[IO]] =
    StaticFile.fromResource(path, blocker, Some(req)).getOrElseF(NotFound())
}
