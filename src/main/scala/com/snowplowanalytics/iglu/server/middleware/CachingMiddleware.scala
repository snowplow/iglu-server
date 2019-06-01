/*
 * Copyright (c) 2019 Snowplow Analytics Ltd. All rights reserved.
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

import cats.Applicative
import cats.data.{ Kleisli, OptionT }
import cats.implicits._
import cats.effect.{Resource, Sync, Async => CatsAsync}

import com.github.benmanes.caffeine.cache.Caffeine

import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import org.http4s.{HttpRoutes, Method, Request, Response}
import org.http4s.syntax.string._

import scalacache.caffeine.CaffeineCache
import scalacache.{Entry, Mode, removeAll, Flags}
import scalacache.CatsEffect.modes._

import scala.concurrent.duration.Duration

object CachingMiddleware  {
  def apply[F[_]](cache: ResponseCache[F])(service: HttpRoutes[F])
                 (implicit F: CatsAsync[F]): HttpRoutes[F] = {
    val logger = Slf4jLogger.getLogger[F]
    Kleisli { req =>
      val result = keyer(req) match {
        case CacheAction.Get(key) =>
          logger.debug(s"Looking up ${key.split(':').toList.headOption.getOrElse("unknown URI")}") *>
            cache.wrap(key)(service(req).value)
        case CacheAction.Clean =>
          logger.debug(s"Cleaning up cache") *>
            service(req).value.flatMap { res =>
              val success = res.exists(_.status.isSuccess)
              if (success) cache.removeSchemas.as(res) else Applicative[F].pure(res)
            }
        case CacheAction.DoNothing =>
          service(req).value
      }
      OptionT(result)
    }
  }

  def initResponseCache[F[_]: CatsAsync](size: Long, duration: Duration): Resource[F, ResponseCache[F]] =
    Resource.make(
      Sync[F].delay {
        val underlyingCaffeineCache =
          Caffeine.newBuilder().maximumSize(size).build[String, Entry[Option[Response[F]]]]
        val cache = CaffeineCache(underlyingCaffeineCache)
        ResponseCache[F](duration, cache)
      })(_.destroy)

  case class ResponseCache[F[_]] private(duration: Duration, schemas: CaffeineCache[Option[Response[F]]]) {

    def removeSchemas(implicit F: CatsAsync[F]): F[Unit] =
      removeAll[Option[Response[F]]]()(schemas, implicitly[Mode[F]]).void

    /** Try to get `key` from a cache. If value is missing, execute `action` and put it into cache if result is success */
    def wrap[A](key: String)(action: F[Option[Response[F]]])(implicit F: CatsAsync[F]): F[Option[Response[F]]] =
      for {
        cacheResult <- schemas.get[F](key)(implicitly[Mode[F]], Flags())
        response <- cacheResult match {
          case Some(cached) =>
            Applicative[F].pure(cached)
          case None =>
            action.flatMap[Option[Response[F]]] {
              case r @ Some(response) if response.status.code >= 200 && response.status.code < 300 =>
                schemas.put(key)(Some(response), Some(getDuration(key))).as(r)
              case other =>
                Applicative[F].pure(other)
            }
        }
      } yield response


    def destroy(implicit F: CatsAsync[F]): F[Unit] =
      schemas.close()(implicitly[Mode[F]]).void

    /** Lower duration for `/schemas/` endpoint */
    def getDuration(key: String) = {
      val path = key.split(":").toList.headOption
        .flatMap { s => Either.catchNonFatal(java.net.URI.create(s)).toOption }
        .map(_.getPath)
        .map(_.stripSuffix("/"))
      val isSchemas = path.contains("/api/schemas")
      if (isSchemas) duration / 12 else duration
    }
  }

  private sealed trait CacheAction
  private object CacheAction {
    case class Get(key: String) extends CacheAction
    case object DoNothing extends CacheAction
    case object Clean extends CacheAction
  }

  /** Make sure that cache works only for GET requests of SchemaService */
  private def keyer[F[_]](req: Request[F]): CacheAction =
    req.method match {
      case Method.GET if req.uri.renderString.contains("/schemas") =>
        val apikey = req.headers.get("apikey".ci).map(_.value).getOrElse("")
        CacheAction.Get(s"${req.uri.renderString}:$apikey")
      case Method.PUT | Method.POST | Method.DELETE =>
        CacheAction.Clean
      case _ =>
        CacheAction.DoNothing
  }
}
