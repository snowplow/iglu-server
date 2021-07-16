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
package com.snowplowanalytics.iglu.server
package middleware

import java.util.UUID

import cats.Applicative
import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.eq._

import org.http4s.{HttpRoutes, Request, Response, Status}
import org.http4s.server.AuthMiddleware
import org.http4s.util.CaseInsensitiveString
import org.http4s.rho.AuthedContext

import com.snowplowanalytics.iglu.server.model.{IgluResponse, Permission}
import com.snowplowanalytics.iglu.server.storage.Storage

/**
  * Used only in HTTP Services, where all endpoints require authentication
  *
  * For any *failed* attempt attempt of authentication, e.g. if apikey is provided,
  * but not found - return 404, "Schema not found" in order to hide the fact
  * of schema existence
  */
object PermissionMiddleware {

  val ApiKey = "apikey"

  /** Build an authentication middleware on top of storage */
  def apply[F[_]: Sync](storage: Storage[F], superKey: Option[UUID]): AuthMiddleware[F, Permission] =
    AuthMiddleware.noSpider(
      Kleisli(request => auth[F](storage, superKey)(request)),
      badRequestHandler
    ) // TODO: SchemaServiceSpec.e6

  /** Extract API key from HTTP request */
  def getApiKey[F[_]](request: Request[F]): Option[Either[Throwable, UUID]] =
    request.headers.get(CaseInsensitiveString(ApiKey)).map(header => header.value).map { apiKey =>
      Either.catchOnly[IllegalArgumentException](UUID.fromString(apiKey))
    }

  def wrapService[F[_]: Sync](
    db: Storage[F],
    superKey: Option[UUID],
    ctx: AuthedContext[F, Permission],
    service: HttpRoutes[F]
  ): HttpRoutes[F] =
    PermissionMiddleware[F](db, superKey).apply(ctx.toService(service))

  private val SchemaNotFoundBody = Utils.toBytes(IgluResponse.SchemaNotFound: IgluResponse)
  private val PermissionsIssue   = Utils.toBytes(IgluResponse.Message("Not enough permissions"): IgluResponse)

  /** Authenticate request against storage */
  private def auth[F[_]: Sync](storage: Storage[F], superKey: Option[UUID])(
    request: Request[F]
  ): OptionT[F, Permission] =
    getApiKey(request) match {
      case None =>
        OptionT.pure(Permission.Noop)
      case Some(Right(apiKey)) =>
        superKey match {
          case Some(uuid) if uuid === apiKey =>
            OptionT.pure(Permission.Super)
          case _ =>
            OptionT(storage.getPermission(apiKey))
        }
      case Some(_) =>
        OptionT.none
    }

  /** Handle invalid apikey as BadRequest, everything else as NotFound
    * (because we don't reveal presence of private resources)
    * Function is called only on Some(_)
    */
  private def badRequestHandler[F[_]](implicit F: Applicative[F]): Request[F] => F[Response[F]] =
    s =>
      getApiKey(s) match {
        case Some(Left(error)) =>
          val body = Utils.toBytes[F, IgluResponse](
            IgluResponse.Message(s"Error parsing apikey HTTP header. ${error.getMessage}")
          )
          F.pure(Response[F](Status.BadRequest, body = body))
        case _ if s.uri.renderString.contains("keygen") => // Horrible way to check if we're not using SchemaService
          F.pure(Response[F](Status.Forbidden, body = PermissionsIssue))
        case Some(Right(_)) =>
          F.pure(Response[F](Status.NotFound, body = SchemaNotFoundBody))
      }
}
