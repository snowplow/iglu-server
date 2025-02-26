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
package storage

import java.util.UUID
import fs2.Stream
import cats.Monad
import cats.effect.{Blocker, Bracket, BracketThrow, Clock, ContextShift, Effect, Resource, Sync}
import cats.implicits._
import io.circe.Json
import doobie.hikari._
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import com.snowplowanalytics.iglu.core.{SchemaMap, SchemaVer}
import com.snowplowanalytics.iglu.server.Config.StorageConfig
import com.snowplowanalytics.iglu.server.model.{Permission, Schema, SchemaDraft}
import com.snowplowanalytics.iglu.server.model.SchemaDraft.DraftId

/**
  * Common interface for supported backend storages
  * Up to implementation to sort schemas by creation date
  */
trait Storage[F[_]] {

  def getSchema(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Option[Schema]]
  def getSchemasByVendor(vendor: String)(implicit F: Bracket[F, Throwable]): Stream[F, Schema] =
    Stream.eval(getSchemas).flatMap(list => Stream.emits(list)).filter(_.schemaMap.schemaKey.vendor === vendor)
  def deleteSchema(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Unit]
  def getSchemasByName(vendor: String, name: String)(implicit F: Bracket[F, Throwable]): Stream[F, Schema] =
    getSchemasByVendor(vendor).filter(_.schemaMap.schemaKey.name === name)
  def getSchemasByModel(vendor: String, name: String, model: Int)(
    implicit F: Bracket[F, Throwable]
  ): Stream[F, Schema] =
    getSchemasByName(vendor, name).filter(_.schemaMap.schemaKey.version.model === model)
  def getSchemas(implicit F: Bracket[F, Throwable]): F[List[Schema]]

  /** Optimization for `getSchemas` */
  def getSchemasKeyOnly(implicit F: Bracket[F, Throwable]): F[List[(SchemaMap, Schema.Metadata)]]
  def getSchemaBody(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Option[Json]] =
    getSchema(schemaMap).nested.map(_.body).value
  def addSchema(schemaMap: SchemaMap, body: Json, isPublic: Boolean, supersedes: List[SchemaVer.Full])(
    implicit C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit]
  def updateSchema(schemaMap: SchemaMap, body: Json, isPublic: Boolean)(
    implicit C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit]

  def addDraft(draftId: DraftId, body: Json, isPublic: Boolean)(implicit C: Clock[F], M: Bracket[F, Throwable]): F[Unit]
  def getDraft(draftId: DraftId)(implicit B: Bracket[F, Throwable]): F[Option[SchemaDraft]]
  def getDrafts(implicit F: Monad[F]): Stream[F, SchemaDraft]

  def getPermission(apikey: UUID)(implicit F: Bracket[F, Throwable]): F[Option[Permission]]
  def addPermission(uuid: UUID, permission: Permission)(implicit F: Bracket[F, Throwable]): F[Unit]
  def addKeyPair(keyPair: Permission.KeyPair, vendor: Permission.Vendor)(implicit F: Bracket[F, Throwable]): F[Unit] =
    for {
      _ <- addPermission(keyPair.read, Permission.ReadOnlyAny.copy(vendor = vendor))
      _ <- addPermission(keyPair.write, Permission.Write.copy(vendor = vendor))
    } yield ()
  def deletePermission(id: UUID)(implicit F: Bracket[F, Throwable]): F[Unit]

  def runAutomaticMigrations(implicit F: Bracket[F, Throwable]): F[Unit]
}

object Storage {

  /** Storage returned an object that cannot be parsed */
  case class IncompatibleStorage(message: String) extends Throwable {
    override def getMessage: String = message
  }

  def initialize[F[_]: Effect: ContextShift](config: StorageConfig): Resource[F, Storage[F]] = {
    val logger = Slf4jLogger.getLoggerFromName("Storage")
    config match {
      case StorageConfig.Dummy =>
        Resource.eval(storage.InMemory.empty)
      case StorageConfig.Postgres(
          host,
          port,
          name,
          username,
          password,
          driver,
          _,
          Config.StorageConfig.ConnectionPool.NoPool(ec),
          enableStartupChecks
          ) =>
        val url = s"jdbc:postgresql://$host:$port/$name"
        for {
          blocker <- Server.createThreadPool(ec).map(Blocker.liftExecutionContext)
          storage <- buildAndCheckPostgres(
            Transactor.fromDriverManager[F](driver, url, username, password, blocker),
            logger,
            enableStartupChecks
          )
        } yield storage
      case p @ StorageConfig.Postgres(
            host,
            port,
            name,
            username,
            password,
            driver,
            _,
            pool: Config.StorageConfig.ConnectionPool.Hikari,
            enableStartupChecks
          ) =>
        val url = s"jdbc:postgresql://$host:$port/$name"
        for {
          connectEC  <- Server.createThreadPool(pool.connectionPool)
          transactEC <- Server.createThreadPool(pool.transactionPool).map(Blocker.liftExecutionContext)
          xa         <- HikariTransactor.newHikariTransactor[F](driver, url, username, password, connectEC, transactEC)
          _ <- Resource.eval {
            xa.configure { ds =>
              Sync[F].delay {
                ds.setPoolName("iglu-hikaricp-pool")

                ds.setMaximumPoolSize(p.maximumPoolSize)
                pool.connectionTimeout.foreach(t => ds.setConnectionTimeout(t.toMillis))
                pool.maxLifetime.foreach(t => ds.setMaxLifetime(t.toMillis))
                pool.minimumIdle.foreach(t => ds.setMinimumIdle(t))
              }
            }
          }
          storage <- buildAndCheckPostgres(xa, logger, enableStartupChecks)
        } yield storage
    }
  }

  private def buildAndCheckPostgres[F[_]: BracketThrow](
    xa: Transactor[F],
    logger: Logger[F],
    enableChecks: Boolean
  ): Resource[F, Postgres[F]] = {
    val pg   = Postgres(xa, logger)
    val pung = if (enableChecks) pg.ping else Monad[F].unit
    Resource.eval(pung.as(pg))
  }

}
