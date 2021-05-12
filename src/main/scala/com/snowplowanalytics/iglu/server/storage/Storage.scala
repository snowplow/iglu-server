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
package storage

import java.util.UUID

import fs2.Stream

import cats.Monad
import cats.effect.{Blocker, Bracket, Clock, ContextShift, Effect, Resource, Sync}
import cats.implicits._

import io.circe.Json

import doobie.hikari._
import doobie.util.transactor.Transactor

import org.typelevel.log4cats.slf4j.Slf4jLogger

import com.snowplowanalytics.iglu.core.SchemaMap
import com.snowplowanalytics.iglu.server.Config.StorageConfig
import com.snowplowanalytics.iglu.server.model.{Permission, Schema, SchemaDraft}
import com.snowplowanalytics.iglu.server.model.SchemaDraft.DraftId

/**
  * Common interface for supported backend storages
  * Up to implementation to sort schemas by creation date
  */
trait Storage[F[_]] {

  def getSchema(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Option[Schema]]
  def getSchemasByVendor(vendor: String, wildcard: Boolean)(implicit F: Bracket[F, Throwable]): Stream[F, Schema] = {
    val all = Stream.eval(getSchemas).flatMap(list => Stream.emits(list))
    if (wildcard) all.filter(_.schemaMap.schemaKey.vendor.startsWith(vendor))
    else all.filter(_.schemaMap.schemaKey.vendor === vendor)
  }
  def deleteSchema(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Unit]
  def getSchemasByVendorName(vendor: String, name: String)(implicit F: Bracket[F, Throwable]): Stream[F, Schema] =
    getSchemasByVendor(vendor, false).filter(_.schemaMap.schemaKey.name === name)
  def getSchemas(implicit F: Bracket[F, Throwable]): F[List[Schema]]

  /** Optimization for `getSchemas` */
  def getSchemasKeyOnly(implicit F: Bracket[F, Throwable]): F[List[(SchemaMap, Schema.Metadata)]]
  def getSchemaBody(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Option[Json]] =
    getSchema(schemaMap).nested.map(_.body).value
  def addSchema(
    schemaMap: SchemaMap,
    body: Json,
    isPublic: Boolean
  )(
    implicit
    C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit]
  def updateSchema(
    schemaMap: SchemaMap,
    body: Json,
    isPublic: Boolean
  )(
    implicit
    C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit]

  def addDraft(
    draftId: DraftId,
    body: Json,
    isPublic: Boolean
  )(
    implicit
    C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit]
  def getDraft(draftId: DraftId)(implicit B: Bracket[F, Throwable]): F[Option[SchemaDraft]]
  def getDrafts(implicit F: Monad[F]): Stream[F, SchemaDraft]

  def getPermission(apikey: UUID)(implicit F: Bracket[F, Throwable]): F[Option[Permission]]
  def addPermission(uuid: UUID, permission: Permission)(implicit F: Bracket[F, Throwable]): F[Unit]
  def addKeyPair(keyPair: Permission.KeyPair, vendor: Permission.Vendor)(implicit F: Bracket[F, Throwable]): F[Unit] =
    for {
      _ <- addPermission(keyPair.read, Permission.ReadOnlyAny.copy(vendor = vendor))
      _ <- addPermission(keyPair.write, Permission.Write.copy(vendor      = vendor))
    } yield ()
  def deletePermission(id: UUID)(implicit F: Bracket[F, Throwable]): F[Unit]
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
          _,
          Config.StorageConfig.ConnectionPool.NoPool(ec)
          ) =>
        val url = s"jdbc:postgresql://$host:$port/$name"
        for {
          blocker <- Server.createThreadPool(ec).map(Blocker.liftExecutionContext)
          xa: Transactor[F] = Transactor.fromDriverManager[F](driver, url, username, password, blocker)
        } yield Postgres[F](xa, logger)
      case p @ StorageConfig.Postgres(
            host,
            port,
            name,
            username,
            password,
            driver,
            _,
            _,
            pool: Config.StorageConfig.ConnectionPool.Hikari
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
                pool.connectionTimeout.foreach(t => ds.setConnectionTimeout(t.toLong))
                pool.maxLifetime.foreach(t       => ds.setMaxLifetime(t.toLong))
                pool.minimumIdle.foreach(t       => ds.setMinimumIdle(t))
              }
            }
          }
          storage <- Resource.eval(Postgres(xa, logger).ping)
        } yield storage
    }
  }
}
