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
import cats.effect.{Blocker, ContextShift, ExitCode, IO, Resource, Sync, Timer}
import cats.effect.concurrent.Ref
import io.circe.syntax._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.Stream
import fs2.concurrent.{Signal, SignallingRef}
import org.http4s.{Headers, HttpApp, HttpRoutes, MediaType, Method, Request, Response, Status}
import org.http4s.headers.{`Content-Type`, `Strict-Transport-Security`}
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{AutoSlash, CORS, EntityLimiter, HSTS, Logger}
import org.http4s.syntax.string._
import org.http4s.server.{defaults => Http4sDefaults}
import org.http4s.util.{CaseInsensitiveString => CIString}
import org.http4s.rho.{AuthedContext, RhoMiddleware}
import org.http4s.rho.bits.PathAST.{PathMatch, TypedPath}
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.http4s.rho.swagger.models.{ApiKeyAuthDefinition, In, Info, SecurityRequirement}
import org.http4s.rho.swagger.SwaggerMetadata
import doobie.implicits._
import doobie.util.transactor.Transactor
import sun.misc.{Signal => JvmSignal, SignalHandler}
import com.snowplowanalytics.iglu.server.migrations.{Bootstrap, MigrateFrom}
import com.snowplowanalytics.iglu.server.codecs.Swagger
import com.snowplowanalytics.iglu.server.middleware.{BadRequestHandler, CachingMiddleware}
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Permission}
import com.snowplowanalytics.iglu.server.storage.Storage
import com.snowplowanalytics.iglu.server.service._
import generated.BuildInfo.version

object Server {

  private val logger = Slf4jLogger.getLogger[IO]

  type RoutesConstructor =
    (Storage[IO], Option[UUID], AuthedContext[IO, Permission], RhoMiddleware[IO]) => HttpRoutes[IO]

  val PermissionContext: AuthedContext[IO, Permission] =
    new AuthedContext[IO, Permission]

  /** TTL for all non-404 responses, cached in memory */
  val CacheTtl = 60.seconds

  /** Default server's response if endpoint was not found */
  val NotFound: Response[IO] =
    Response[IO]()
      .withStatus(Status.NotFound)
      .withBodyStream(Utils.toBytes(IgluResponse.EndpointNotFound: IgluResponse))
      .withContentType(`Content-Type`(MediaType.application.json))

  val PayloadTooLarge: Response[IO] =
    Response[IO]()
      .withStatus(Status.PayloadTooLarge)
      .withBodyStream(Utils.toBytes(IgluResponse.PayloadTooLarge: IgluResponse))
      .withContentType(`Content-Type`(MediaType.application.json))

  def addSwagger(storage: Storage[IO], superKey: Option[UUID], config: Config.Swagger)(
    service: (String, RoutesConstructor)
  ) = {
    val (base, constructor) = service
    val swagger = ioSwagger.createRhoMiddleware(
      jsonApiPath = TypedPath(PathMatch("swagger.json")),
      swaggerMetadata = SwaggerMetadata(
        apiInfo = Info(title = "Iglu Server API", version = version),
        basePath = Some(config.baseUrl.stripSuffix("/") ++ base),
        securityDefinitions = Map("Iglu API key" -> ApiKeyAuthDefinition("apikey", In.HEADER)),
        security = List(SecurityRequirement("Iglu API key", Nil))
      ),
      swaggerFormats = Swagger.Formats
    )

    base -> constructor(storage, superKey, PermissionContext, swagger)
  }

  def httpApp(
    storage: Storage[IO],
    superKey: Option[UUID],
    debug: Boolean,
    patchesAllowed: Boolean,
    webhook: Webhook.WebhookClient[IO],
    cache: CachingMiddleware.ResponseCache[IO],
    swaggerConfig: Config.Swagger,
    blocker: Blocker,
    isHealthy: IO[Boolean],
    hsts: Config.Hsts,
    maxPayloadSize: Long,
    maxJsonDepth: Int
  )(implicit cs: ContextShift[IO]): HttpApp[IO] = {
    val serverRoutes =
      httpRoutes(
        storage,
        superKey,
        debug,
        patchesAllowed,
        webhook,
        cache,
        swaggerConfig,
        blocker,
        isHealthy,
        maxJsonDepth
      )
    val server = Kleisli[IO, Request[IO], Response[IO]](req => Router(serverRoutes: _*).run(req).getOrElse(NotFound))
    entityLimiter(maxPayloadSize)(hstsMiddleware(hsts)(server))
  }

  def hstsMiddleware(hsts: Config.Hsts): HttpApp[IO] => HttpApp[IO] =
    if (hsts.enable)
      HSTS(_, `Strict-Transport-Security`.unsafeFromDuration(hsts.maxAge))
    else identity

  def entityLimiter(maxPayloadSize: Long): HttpApp[IO] => HttpApp[IO] =
    EntityLimiter(_, maxPayloadSize).recover {
      case _: EntityLimiter.EntityTooLarge => PayloadTooLarge
    }

  def httpRoutes(
    storage: Storage[IO],
    superKey: Option[UUID],
    debug: Boolean,
    patchesAllowed: Boolean,
    webhook: Webhook.WebhookClient[IO],
    cache: CachingMiddleware.ResponseCache[IO],
    swaggerConfig: Config.Swagger,
    blocker: Blocker,
    isHealthy: IO[Boolean],
    maxJsonDepth: Int
  )(implicit cs: ContextShift[IO]): List[(String, HttpRoutes[IO])] = {
    val services: List[(String, RoutesConstructor)] = List(
      "/api/meta"       -> MetaService.asRoutes(debug, patchesAllowed, isHealthy),
      "/api/schemas"    -> SchemaService.asRoutes(patchesAllowed, webhook, maxJsonDepth),
      "/api/auth"       -> AuthService.asRoutes,
      "/api/validation" -> ValidationService.asRoutes(maxJsonDepth),
      "/api/drafts"     -> DraftService.asRoutes
    )

    val debugRoute  = "/api/debug" -> DebugService.asRoutes(storage, ioSwagger.createRhoMiddleware())
    val staticRoute = "/static" -> StaticService.routes(blocker)
    val routes      = staticRoute :: services.map(addSwagger(storage, superKey, swaggerConfig))
    val corsPolicy = CORS
      .policy
      .withAllowOriginHostCi(_ => true)
      .withAllowCredentials(false)
      .withAllowMethodsIn(Set(Method.GET, Method.POST, Method.PUT, Method.OPTIONS, Method.DELETE))
      .withAllowHeadersIn(Set(CIString("content-type"), CIString("apikey")))
      .withMaxAge(1.day)

    (if (debug) debugRoute :: routes else routes).map {
      case (endpoint, route) =>
        // Apply middleware
        val httpRoutes        = CachingMiddleware(cache)(BadRequestHandler(corsPolicy(AutoSlash(route))))
        val redactHeadersWhen = (Headers.SensitiveHeaders + "apikey".ci).contains _
        (endpoint, Logger.httpRoutes[IO](true, true, redactHeadersWhen, Some(logger.debug(_)))(httpRoutes))
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

  def buildServer(
    config: Config,
    isHealthy: IO[Boolean]
  )(implicit cs: ContextShift[IO], timer: Timer[IO]): Resource[IO, BlazeServerBuilder[IO]] =
    for {
      _        <- Resource.eval(logger.info(s"Initializing server with following configuration: ${config.asJson.noSpaces}"))
      httpPool <- createThreadPool[IO](config.repoServer.threadPool)
      client   <- BlazeClientBuilder[IO](httpPool).resource
      webhookClient = Webhook.WebhookClient(config.webhooks, client)
      storage <- Storage.initialize[IO](config.database)
      _       <- Resource.eval(storage.runAutomaticMigrations)
      cache   <- CachingMiddleware.initResponseCache[IO](1000, CacheTtl)
      blocker <- Blocker[IO],
    } yield BlazeServerBuilder[IO](httpPool)
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
          blocker,
          isHealthy,
          config.repoServer.hsts,
          config.repoServer.maxPayloadSize,
          config.maxJsonDepth
        )
      )
      .withIdleTimeout(config.repoServer.idleTimeout.getOrElse(Http4sDefaults.IdleTimeout))
      .withMaxConnections(config.repoServer.maxConnections.getOrElse(Http4sDefaults.MaxConnections))

  def run(config: Config)(implicit cs: ContextShift[IO], timer: Timer[IO]): IO[ExitCode] =
    runStream(config).compile.lastOrError

  def runStream(config: Config)(implicit cs: ContextShift[IO], timer: Timer[IO]): Stream[IO, ExitCode] =
    for {
      sigToExit   <- Stream.eval(SignallingRef[IO, Boolean](false))
      sigToPause  <- Stream.eval(SignallingRef[IO, Boolean](false))
      refExitCode <- Stream.eval(Ref[IO].of(ExitCode.Success))
      _           <- Stream.eval(addShutdownHook(sigToPause))
      builder     <- Stream.resource(buildServer(config, getIsHealthy(config, sigToPause)))
      exitCode    <- builder.serveWhile(sigToExit, refExitCode).concurrently(handleSigTerm(config, sigToPause, sigToExit))
    } yield exitCode

  def handleSigTerm(config: Config, sigToPause: Signal[IO, Boolean], sigToExit: Ref[IO, Boolean])(
    implicit timer: Timer[IO]
  ): Stream[IO, Unit] =
    sigToPause.discrete.evalMap {
      case true =>
        // We got a SIGTERM
        for {
          _ <- logger.warn(
            s"Initiating server shutdown. Sleeping for ${config.preTerminationPeriod} as part of graceful shutdown."
          )
          _ <- IO.sleep(config.preTerminationPeriod)
          _ <- logger.warn("Terminating the server")
          _ <- sigToExit.set(true)
        } yield ()
      case false =>
        IO.unit
    }

  def getIsHealthy(config: Config, sigToPause: Ref[IO, Boolean]): IO[Boolean] =
    if (config.preTerminationUnhealthy)
      sigToPause.get.map { isTerminating =>
        // healthy if not terminating
        !isTerminating
      }
    else IO.pure(true)

  def addShutdownHook(received: Ref[IO, Boolean]): IO[Unit] =
    IO.delay {
      val handler = new SignalHandler {
        override def handle(signal: JvmSignal): Unit =
          received.set(true).unsafeRunSync()
      }
      JvmSignal.handle(new JvmSignal("TERM"), handler)
      ()
    }

  def setup(config: Config, migrate: Option[MigrateFrom])(implicit cs: ContextShift[IO]): IO[ExitCode] =
    config.database match {
      case pg: Config.StorageConfig.Postgres =>
        val xa = getTransactor(pg)
        val action = migrate match {
          case Some(migration) =>
            migration.perform.transact(xa) *>
              logger.warn(s"All tables were migrated in ${pg.dbname} from $migration")
          case None =>
            Bootstrap.initialize[IO](xa) *>
              logger.warn(s"Tables were initialized in ${pg.dbname}")
        }
        action.as(ExitCode.Success)
      case Config.StorageConfig.Dummy =>
        logger.error(s"Nothing to setup with dummy storage").as(ExitCode.Error)
    }

  def getTransactor(config: Config.StorageConfig.Postgres)(implicit cs: ContextShift[IO]): Transactor[IO] = {
    val url = s"jdbc:postgresql://${config.host}:${config.port}/${config.dbname}"
    Transactor.fromDriverManager[IO](config.driver, url, config.username, config.password)
  }
}
