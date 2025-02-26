/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server
package middleware

import java.util.UUID

import cats.Applicative
import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.functor._
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

  private val ApiKeyIssue = Utils.toBytes(IgluResponse.Message("Invalid UUID for ApiKey"): IgluResponse)

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
            // An unknown api key is treated the same as a missing api key.
            // This prevents leaking information about what api keys exist.
            OptionT.liftF(storage.getPermission(apiKey).map(_.getOrElse(Permission.Noop)))
        }
      case Some(_) =>
        OptionT.none
    }

  /** This handler is only called if a UUID cannot be extracted from the apiKey header
    *
    *  The service itself handles the case when permissions are not sufficient.
    */
  private def badRequestHandler[F[_]](implicit F: Applicative[F]): Request[F] => F[Response[F]] =
    _ => F.pure(Response[F](Status.BadRequest, body = ApiKeyIssue))
}
