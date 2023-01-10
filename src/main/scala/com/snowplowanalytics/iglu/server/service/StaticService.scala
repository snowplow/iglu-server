/*
 * Copyright (c) 2019-2023 Snowplow Analytics Ltd. All rights reserved.
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
