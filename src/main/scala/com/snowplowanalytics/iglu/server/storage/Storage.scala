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
import cats.effect.{Async, Resource, Sync}
import cats.implicits._
import io.circe.Json
import doobie.hikari.HikariTransactor
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
abstract class Storage[F[_]: Sync] {

  def getSchema(schemaMap: SchemaMap): F[Option[Schema]]
  def getSchemasByVendor(vendor: String): Stream[F, Schema] =
    Stream.eval(getSchemas).flatMap(list => Stream.emits(list)).filter(_.schemaMap.schemaKey.vendor === vendor)
  def deleteSchema(schemaMap: SchemaMap): F[Unit]
  def getSchemasByName(vendor: String, name: String): Stream[F, Schema] =
    getSchemasByVendor(vendor).filter(_.schemaMap.schemaKey.name === name)
  def getSchemasByModel(vendor: String, name: String, model: Int): Stream[F, Schema] =
    getSchemasByName(vendor, name).filter(_.schemaMap.schemaKey.version.model === model)
  def getSchemas: F[List[Schema]]

  /** Optimization for `getSchemas` */
  def getSchemasKeyOnly: F[List[(SchemaMap, Schema.Metadata)]]
  def getSchemaBody(schemaMap: SchemaMap): F[Option[Json]] =
    getSchema(schemaMap).nested.map(_.body).value

  /**
    * Atomically insert a schema or, on primary-key conflict, either update it (when `patchesAllowed`)
    * or leave the existing row untouched (when not). This is the single write path for schemas: it
    * replaces the racy check-then-act sequence of `getSchema` + insert/update with one statement, so
    * concurrent uploads of the same version cannot bypass `patchesAllowed`, create duplicates, or deadlock.
    *
    * @return `true` if a new row was inserted, `false` if the row already existed (updated when
    *         `patchesAllowed`, or left untouched otherwise)
    */
  def upsertSchema(
    schemaMap: SchemaMap,
    body: Json,
    isPublic: Boolean,
    supersedes: List[SchemaVer.Full],
    patchesAllowed: Boolean
  ): F[Boolean]

  def addDraft(draftId: DraftId, body: Json, isPublic: Boolean): F[Unit]
  def getDraft(draftId: DraftId): F[Option[SchemaDraft]]
  def getDrafts: Stream[F, SchemaDraft]

  def getPermission(apikey: UUID): F[Option[Permission]]
  def addPermission(uuid: UUID, permission: Permission): F[Unit]
  def addKeyPair(keyPair: Permission.KeyPair, vendor: Permission.Vendor): F[Unit] =
    for {
      _ <- addPermission(keyPair.read, Permission.ReadOnlyAny.copy(vendor = vendor))
      _ <- addPermission(keyPair.write, Permission.Write.copy(vendor = vendor))
    } yield ()
  def deletePermission(id: UUID): F[Unit]

  def runAutomaticMigrations: F[Unit]
}

object Storage {

  /** Storage returned an object that cannot be parsed */
  case class IncompatibleStorage(message: String) extends Throwable {
    override def getMessage: String = message
  }

  def initialize[F[_]: Async](config: StorageConfig): Resource[F, Storage[F]] = {
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
          Config.StorageConfig.ConnectionPool.NoPool,
          enableStartupChecks
          ) =>
        val url = s"jdbc:postgresql://$host:$port/$name"
        buildAndCheckPostgres(
          Transactor.fromDriverManager[F](driver, url, username, password, None),
          logger,
          enableStartupChecks
        )
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
          connectEC <- Server.createThreadPool(pool.connectionPool)
          xa        <- HikariTransactor.newHikariTransactor[F](driver, url, username, password, connectEC)
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

  private def buildAndCheckPostgres[F[_]: Async](
    xa: Transactor[F],
    logger: Logger[F],
    enableChecks: Boolean
  ): Resource[F, Postgres[F]] = {
    val pg   = Postgres(xa, logger)
    val pung = if (enableChecks) pg.ping else Monad[F].unit
    Resource.eval(pung.as(pg))
  }

}
