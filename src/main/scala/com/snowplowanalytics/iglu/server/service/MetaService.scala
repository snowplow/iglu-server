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

import java.util.UUID

import cats.effect.{IO, Sync}
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.flatMap._

import io.circe._
import io.circe.generic.semiauto.deriveEncoder

import org.http4s.{Charset, HttpRoutes, MediaType}
import org.http4s.headers.`Content-Type`
import org.http4s.rho.{AuthedContext, RhoMiddleware, RhoRoutes}
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.rho.swagger.syntax.{io => swaggerSyntax}

import com.snowplowanalytics.iglu.server.codecs.JsonCodecs._
import com.snowplowanalytics.iglu.server.generated.BuildInfo
import com.snowplowanalytics.iglu.server.model.Permission
import com.snowplowanalytics.iglu.server.storage.{InMemory, Postgres, Storage}
import com.snowplowanalytics.iglu.server.middleware.PermissionMiddleware

class MetaService[F[+_]: Sync](
  debug: Boolean,
  patchesAllowed: Boolean,
  swagger: SwaggerSyntax[F],
  ctx: AuthedContext[F, Permission],
  db: Storage[F],
  isHealthy: F[Boolean]
) extends RhoRoutes[F] {
  import swagger._

  private val ok = Ok("OK").map(_.withContentType(`Content-Type`(MediaType.text.plain, Charset.`UTF-8`)))

  "This route responds with OK string when the server is healthy" **
    GET / "health" |>> isHealthy.flatMap {
    case true => ok
    case false =>
      ServiceUnavailable("Service Unavailable").map(
        _.withContentType(`Content-Type`(MediaType.text.plain, Charset.`UTF-8`))
      )
  }

  "This route responds with OK string if database is available" **
    GET / "health" / "db" |>> {
    for {
      _ <- db match {
        case pg: Postgres[F] => pg.ping.void
        case _               => Sync[F].unit
      }
      response <- ok
    } yield response
  }

  "This route responds with info about the Iglu Server" **
    GET / "server" >>> ctx.auth |>> { authInfo: Permission =>
    val database = db match {
      case _: Postgres[F] => "postgres"
      case _: InMemory[F] => "inmemory"
      case _              => "unknown"
    }
    for {
      schemas <- db.getSchemasKeyOnly
      count = schemas.filter(s => authInfo.canRead(s._1.schemaKey.vendor)).as(()).length
      response <- Ok(MetaService.ServerInfo(BuildInfo.version, authInfo, database, count, debug, patchesAllowed))
    } yield response
  }
}

object MetaService {
  case class ServerInfo(
    version: String,
    authInfo: Permission,
    database: String,
    schemaCount: Int,
    debug: Boolean,
    patchesAllowed: Boolean
  )

  implicit val serverInfoEncoderInstance: Encoder[ServerInfo] = deriveEncoder[ServerInfo]

  def asRoutes(
    debug: Boolean,
    patchesAllowed: Boolean,
    isHealthy: IO[Boolean]
  )(
    db: Storage[IO],
    superKey: Option[UUID],
    ctx: AuthedContext[IO, Permission],
    rhoMiddleware: RhoMiddleware[IO]
  ): HttpRoutes[IO] = {
    val service = new MetaService(debug, patchesAllowed, swaggerSyntax, ctx, db, isHealthy).toRoutes(rhoMiddleware)
    PermissionMiddleware.wrapService(db, superKey, ctx, service)
  }
}
