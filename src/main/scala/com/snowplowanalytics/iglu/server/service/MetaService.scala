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
import cats.syntax.either._
import cats.syntax.functor._

import io.circe._
import io.circe.generic.semiauto.deriveEncoder

import sttp.model.StatusCode
import sttp.tapir.{PublicEndpoint, stringBody, stringToPath}

import com.snowplowanalytics.iglu.server.generated.BuildInfo
import com.snowplowanalytics.iglu.server.http.Api
import com.snowplowanalytics.iglu.server.http.Endpoints._
import com.snowplowanalytics.iglu.server.http.Schemas._
import com.snowplowanalytics.iglu.server.model.Permission
import com.snowplowanalytics.iglu.server.storage.{InMemory, Postgres, Storage}

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

  private val ok = (StatusCode.Ok, "OK")

  def health: PublicEndpoint[Unit, Failure, (StatusCode, String), Any] =
    base()
      .get
      .in("health")
      .out(statuses(StatusCode.Ok, StatusCode.ServiceUnavailable).and(stringBody))
      .summary("This route responds with OK string when the server is healthy")

  def healthDb: PublicEndpoint[Unit, Failure, (StatusCode, String), Any] =
    base()
      .get
      .in("health" / "db")
      .out(statuses(StatusCode.Ok).and(stringBody))
      .summary("This route responds with OK string if database is available")

  def server: PublicEndpoint[Unit, Failure, ServerInfo, Any] =
    base().get.in("server").out(jsonOut[ServerInfo]).summary("This route responds with info about the Iglu Server")

  def api[F[_]: Async](debug: Boolean, patchesAllowed: Boolean, isHealthy: F[Boolean])(db: Storage[F]): Api[F] =
    Api(
      List(
        Api.withPermission(health) { _ => _ =>
          isHealthy.map(healthy =>
            (if (healthy) ok else (StatusCode.ServiceUnavailable, "Service Unavailable")).asRight
          )
        },
        Api.withPermission(healthDb) { _ => _ =>
          val ping = db match {
            case pg: Postgres[F] => pg.ping.void
            case _               => Async[F].unit
          }
          ping.as(ok.asRight)
        },
        Api.withPermission(server) { authInfo => _ =>
          val database = db match {
            case _: Postgres[F] => "postgres"
            case _: InMemory[F] => "inmemory"
            case _              => "unknown"
          }
          db.getSchemasKeyOnly.map { schemas =>
            val count = schemas.count(s => authInfo.canRead(s._1.schemaKey.vendor))
            ServerInfo(BuildInfo.version, authInfo, database, count, debug, patchesAllowed).asRight
          }
        }
      )
    )

}
