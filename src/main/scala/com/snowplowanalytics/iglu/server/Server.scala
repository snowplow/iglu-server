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

import java.util.concurrent.{ExecutorService, Executors}
import java.util.UUID
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import cats.data.Kleisli
import cats.syntax.all._
import cats.effect.{Async, ExitCode, Ref, Resource, Sync}
import cats.effect.std.Dispatcher
import io.circe.syntax._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.Stream
import fs2.concurrent.{Signal, SignallingRef}
import org.http4s.{AuthedRequest, Headers, HttpApp, HttpRoutes, MediaType, Method, Request, Response, Status}
import org.http4s.headers.{`Content-Type`, `Strict-Transport-Security`}
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.{DefaultServiceErrorHandler, Router, ServiceErrorHandler}
import org.http4s.server.middleware.{AutoSlash, CORS, EntityLimiter, HSTS, Logger => RequestLogger}
import org.http4s.server.{defaults => Http4sDefaults}
import org.typelevel.ci.CIString
import doobie.implicits._
import doobie.util.log.{ExecFailure, LogEvent, LogHandler, ProcessingFailure, Success}
import doobie.util.transactor.Transactor
import sun.misc.{Signal => JvmSignal, SignalHandler}
import com.snowplowanalytics.iglu.server.migrations.{Bootstrap, MigrateFrom}
import com.snowplowanalytics.iglu.server.middleware.{BadRequestHandler, CachingMiddleware, PermissionMiddleware}
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Permission}
import com.snowplowanalytics.iglu.server.storage.{Postgres, Storage}
import com.snowplowanalytics.iglu.server.service._

object Server {

  private def logger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  /** TTL for all non-404 responses, cached in memory */
  val CacheTtl = 60.seconds

  /** Default server's response if endpoint was not found */
  def NotFound[F[_]]: Response[F] =
    Response[F]()
      .withStatus(Status.NotFound)
      .withBodyStream(Utils.toBytes(IgluResponse.EndpointNotFound: IgluResponse))
      .withContentType(`Content-Type`(MediaType.application.json))

  def PayloadTooLarge[F[_]]: Response[F] =
    Response[F]()
      .withStatus(Status.PayloadTooLarge)
      .withBodyStream(Utils.toBytes(IgluResponse.PayloadTooLarge: IgluResponse))
      .withContentType(`Content-Type`(MediaType.application.json))

  def ServiceUnavailable[F[_]]: Response[F] =
    Response[F]()
      .withStatus(Status.ServiceUnavailable)
      .withBodyStream(Utils.toBytes(IgluResponse.Message("Transient database error, please retry"): IgluResponse))
      .withContentType(`Content-Type`(MediaType.application.json))

  /**
    * 503 for transient database errors that persisted despite the retries, as the client can retry
    * the request. Default handler (500) for any other error.
    */
  def serviceErrorHandler[F[_]: Sync]: ServiceErrorHandler[F] = {
    val log = logger[F]
    request => {
      val transientError: PartialFunction[Throwable, F[Response[F]]] = {
        case Postgres.TransientError(e) =>
          log
            .error(e)(s"Transient database error servicing request: ${request.method} ${request.pathInfo}")
            .as(ServiceUnavailable[F])
      }
      transientError.orElse(DefaultServiceErrorHandler[F].apply(request))
    }
  }

  /** Server URL of the routes mounted at `prefix`, in their OpenAPI documentation */
  def docsBasePath(config: Config.Swagger, prefix: String): String =
    config.baseUrl.stripSuffix("/") ++ prefix

  def httpApp[F[_]: Async](
    storage: Storage[F],
    superKey: Option[UUID],
    debug: Boolean,
    patchesAllowed: Boolean,
    webhook: Webhook.WebhookClient[F],
    cache: CachingMiddleware.ResponseCache[F],
    swaggerConfig: Config.Swagger,
    isHealthy: F[Boolean],
    hsts: Config.Hsts,
    maxPayloadSize: Long,
    maxJsonDepth: Int
  ): HttpApp[F] = {
    val serverRoutes =
      httpRoutes(
        storage,
        superKey,
        debug,
        patchesAllowed,
        webhook,
        cache,
        swaggerConfig,
        isHealthy,
        maxJsonDepth
      )
    val server = Kleisli[F, Request[F], Response[F]](req => Router(serverRoutes: _*).run(req).getOrElse(NotFound[F]))
    entityLimiter[F](maxPayloadSize).apply(hstsMiddleware[F](hsts).apply(server))
  }

  def hstsMiddleware[F[_]: Sync](hsts: Config.Hsts): HttpApp[F] => HttpApp[F] =
    if (hsts.enable)
      HSTS(_, `Strict-Transport-Security`.unsafeFromDuration(hsts.maxAge))
    else identity

  def entityLimiter[F[_]: Sync](maxPayloadSize: Long): HttpApp[F] => HttpApp[F] =
    EntityLimiter(_, maxPayloadSize).recover {
      case _: EntityLimiter.EntityTooLarge => PayloadTooLarge[F]
    }

  def httpRoutes[F[_]: Async](
    storage: Storage[F],
    superKey: Option[UUID],
    debug: Boolean,
    patchesAllowed: Boolean,
    webhook: Webhook.WebhookClient[F],
    cache: CachingMiddleware.ResponseCache[F],
    swaggerConfig: Config.Swagger,
    isHealthy: F[Boolean],
    maxJsonDepth: Int
  ): List[(String, HttpRoutes[F])] = {
    val services = List(
      "/api/meta"       -> MetaService.api(debug, patchesAllowed, isHealthy)(storage),
      "/api/schemas"    -> SchemaService.api(patchesAllowed, webhook, maxJsonDepth)(storage),
      "/api/auth"       -> AuthService.api(maxJsonDepth)(storage),
      "/api/validation" -> ValidationService.api(maxJsonDepth)(storage),
      "/api/drafts"     -> DraftService.api(maxJsonDepth)(storage)
    )

    // Not authenticated
    val debugRoute = "/api/debug" -> DebugService
      .api(storage)
      .routes(docsBasePath(swaggerConfig, "/api/debug"))
      .local[Request[F]](AuthedRequest(Permission.Noop, _))
    val staticRoute = "/static" -> StaticService.routes[F]
    val routes = staticRoute :: services.map {
      case (prefix, api) =>
        prefix -> PermissionMiddleware.wrapService(storage, superKey, api.routes(docsBasePath(swaggerConfig, prefix)))
    }
    val corsPolicy = CORS
      .policy
      .withAllowOriginHostCi(_ => true)
      .withAllowCredentials(false)
      .withAllowMethodsIn(Set(Method.GET, Method.POST, Method.PUT, Method.OPTIONS, Method.DELETE))
      .withAllowHeadersIn(Set(CIString("content-type"), CIString(PermissionMiddleware.ApiKey)))
      .withMaxAge(1.day)

    val log = logger[F]
    (if (debug) debugRoute :: routes else routes).map {
      case (endpoint, route) =>
        // Apply middleware
        val httpRoutes        = CachingMiddleware(cache)(BadRequestHandler(corsPolicy(AutoSlash(route))))
        val redactHeadersWhen = (Headers.SensitiveHeaders + CIString(PermissionMiddleware.ApiKey)).contains _
        (endpoint, RequestLogger.httpRoutes[F](true, true, redactHeadersWhen, Some(log.debug(_)))(httpRoutes))
    }
  }

  def createThreadPool[F[_]: Sync](pool: Config.ThreadPool): Resource[F, ExecutionContext] =
    pool match {
      case Config.ThreadPool.Global => // Assuming we already have shutdown hook thanks to IOApp
        Resource.pure[F, ExecutionContext](scala.concurrent.ExecutionContext.global)
      case Config.ThreadPool.Cached =>
        val alloc = Sync[F].delay(Executors.newCachedThreadPool)
        val free  = (es: ExecutorService) => Sync[F].delay(es.shutdown())
        Resource.make(alloc)(free).map(ExecutionContext.fromExecutor)
      case Config.ThreadPool.Fixed(size) =>
        val alloc = Sync[F].delay(Executors.newFixedThreadPool(size))
        val free  = (es: ExecutorService) => Sync[F].delay(es.shutdown())
        Resource.make(alloc)(free).map(ExecutionContext.fromExecutor)
    }

  def buildServer[F[_]: Async](
    config: Config,
    isHealthy: F[Boolean]
  ): Resource[F, BlazeServerBuilder[F]] =
    for {
      _        <- Resource.eval(logger[F].info(s"Initializing server with following configuration: ${config.asJson.noSpaces}"))
      httpPool <- createThreadPool[F](config.repoServer.threadPool)
      client   <- BlazeClientBuilder[F].withExecutionContext(httpPool).resource
      webhookClient = Webhook.WebhookClient(config.webhooks, client)
      storage <- Storage.initialize[F](config.database)
      _       <- Resource.eval(storage.runAutomaticMigrations)
      cache   <- CachingMiddleware.initResponseCache[F](1000, CacheTtl)
    } yield BlazeServerBuilder[F]
      .withExecutionContext(httpPool)
      .bindHttp(config.repoServer.port, config.repoServer.interface)
      .withHttpApp(
        httpApp(
          storage,
          config.superApiKey,
          config.debug,
          config.patchesAllowed,
          webhookClient,
          cache,
          config.swagger,
          isHealthy,
          config.repoServer.hsts,
          config.repoServer.maxPayloadSize,
          config.maxJsonDepth
        )
      )
      .withServiceErrorHandler(serviceErrorHandler[F])
      .withIdleTimeout(config.repoServer.idleTimeout.getOrElse(Http4sDefaults.IdleTimeout))
      .withMaxConnections(config.repoServer.maxConnections.getOrElse(Http4sDefaults.MaxConnections))

  def run[F[_]: Async](config: Config): F[ExitCode] =
    runStream[F](config).compile.lastOrError

  def runStream[F[_]: Async](config: Config): Stream[F, ExitCode] =
    for {
      sigToExit   <- Stream.eval(SignallingRef[F, Boolean](false))
      sigToPause  <- Stream.eval(SignallingRef[F, Boolean](false))
      refExitCode <- Stream.eval(Ref[F].of(ExitCode.Success))
      dispatcher  <- Stream.resource(Dispatcher.sequential[F])
      _           <- Stream.resource(addShutdownHook(sigToPause, dispatcher))
      builder     <- Stream.resource(buildServer(config, getIsHealthy(config, sigToPause)))
      exitCode    <- builder.serveWhile(sigToExit, refExitCode).concurrently(handleSigTerm(config, sigToPause, sigToExit))
    } yield exitCode

  def handleSigTerm[F[_]: Async](
    config: Config,
    sigToPause: Signal[F, Boolean],
    sigToExit: Ref[F, Boolean]
  ): Stream[F, Unit] =
    sigToPause.discrete.evalMap {
      case true =>
        // We got a SIGTERM
        for {
          _ <- logger[F].warn(
            s"Initiating server shutdown. Sleeping for ${config.preTerminationPeriod} as part of graceful shutdown."
          )
          _ <- Async[F].sleep(config.preTerminationPeriod)
          _ <- logger[F].warn("Terminating the server")
          _ <- sigToExit.set(true)
        } yield ()
      case false =>
        Async[F].unit
    }

  def getIsHealthy[F[_]: Sync](config: Config, sigToPause: Ref[F, Boolean]): F[Boolean] =
    if (config.preTerminationUnhealthy)
      sigToPause.get.map { isTerminating =>
        // healthy if not terminating
        !isTerminating
      }
    else Sync[F].pure(true)

  def addShutdownHook[F[_]: Sync](received: Ref[F, Boolean], dispatcher: Dispatcher[F]): Resource[F, Unit] = {
    val term = new JvmSignal("TERM")
    val handler = new SignalHandler {
      override def handle(signal: JvmSignal): Unit =
        dispatcher.unsafeRunAndForget(received.set(true))
    }
    Resource
      .make(Sync[F].delay(JvmSignal.handle(term, handler)))(previous =>
        Sync[F].delay(JvmSignal.handle(term, previous)).void
      )
      .void
  }

  def setup[F[_]: Async](config: Config, migrate: Option[MigrateFrom]): F[ExitCode] =
    config.database match {
      case pg: Config.StorageConfig.Postgres =>
        val xa = getTransactor[F](pg)
        val action = migrate match {
          case Some(migration) =>
            migration.perform.transact(xa) *>
              logger[F].warn(s"All tables were migrated in ${pg.dbname} from $migration")
          case None =>
            Bootstrap.initialize[F](xa) *>
              logger[F].warn(s"Tables were initialized in ${pg.dbname}")
        }
        action.as(ExitCode.Success)
      case Config.StorageConfig.Dummy =>
        logger[F].error(s"Nothing to setup with dummy storage").as(ExitCode.Error)
    }

  /** The statements run by `setup` are logged */
  def getTransactor[F[_]: Async](config: Config.StorageConfig.Postgres): Transactor[F] = {
    val url = s"jdbc:postgresql://${config.host}:${config.port}/${config.dbname}"
    Transactor.fromDriverManager[F](config.driver, url, config.username, config.password, Some(statementLogHandler[F]))
  }

  /** Log the statements, but not their parameters, which can be API keys (e.g. when migrating permissions) */
  def statementLogHandler[F[_]: Sync]: LogHandler[F] = {
    val log = logger[F]
    new LogHandler[F] {
      def run(event: LogEvent): F[Unit] = {
        val sql = event.sql.linesIterator.map(_.trim).filter(_.nonEmpty).mkString(" ")
        event match {
          case e: Success =>
            log.info(s"Successful statement execution (${e.exec.toMillis} ms): $sql")
          case e: ExecFailure =>
            log.error(e.failure)(s"Failed statement execution (${e.exec.toMillis} ms): $sql")
          case e: ProcessingFailure =>
            log.error(e.failure)(s"Failed result set processing (${e.exec.toMillis} ms): $sql")
        }
      }
    }
  }
}
