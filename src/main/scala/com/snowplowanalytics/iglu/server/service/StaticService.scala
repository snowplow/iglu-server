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

import cats.effect.Sync

import org.http4s.{HttpRoutes, Request, Response, StaticFile}
import org.http4s.dsl.Http4sDsl

import com.snowplowanalytics.iglu.server.generated.BuildInfo

object StaticService {
  private val localUi      = "/swagger-ui-dist"
  private val swaggerUiDir = s"/META-INF/resources/webjars/swagger-ui/${BuildInfo.SwaggerUI}"

  /**
    * Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
    * but its nice to keep it self contained
    */
  def routes[F[_]: Sync]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    def fetchResource(path: String, req: Request[F]): F[Response[F]] =
      StaticFile.fromResource(path, Some(req)).getOrElseF(NotFound())

    HttpRoutes.of[F] {
      // Swagger User Interface
      case req @ GET -> Root / "swagger-ui" / "index.html" => fetchResource(localUi + "/index.html", req)
      case req @ GET -> Root / "swagger-ui" / path         => fetchResource(swaggerUiDir + "/" + path, req)
    }
  }
}
