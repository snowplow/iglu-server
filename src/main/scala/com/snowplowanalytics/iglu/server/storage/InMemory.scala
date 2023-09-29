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
package com.snowplowanalytics.iglu.server.storage

import java.util.UUID
import java.util.concurrent.TimeUnit
import java.time.Instant
import fs2.Stream
import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.{Bracket, Clock, Sync}
import cats.effect.concurrent.Ref
import io.circe.Json
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer}
import com.snowplowanalytics.iglu.server.model.Schema.SupersedingInfo
import com.snowplowanalytics.iglu.server.model.{Permission, Schema, SchemaDraft}
import com.snowplowanalytics.iglu.server.model.SchemaDraft.DraftId

/** Ephemeral storage that will be lost after server shut down */
case class InMemory[F[_]](ref: Ref[F, InMemory.State]) extends Storage[F] {
  def getSchema(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Option[Schema]] =
    for { db <- ref.get } yield db.schemas.get(schemaMap).map(addSupersedingInfo(db))

  def getPermission(apikey: UUID)(implicit F: Bracket[F, Throwable]): F[Option[Permission]] =
    for { db <- ref.get } yield db.permission.get(apikey)

  def deleteSchema(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Unit] =
    for {
      db <- ref.get
      newState = db.copy(schemas = db.schemas - schemaMap)
      _ <- ref.set(newState)
    } yield ()

  def addSchema(schemaMap: SchemaMap, body: Json, isPublic: Boolean, supersedes: List[SchemaVer.Full])(
    implicit C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit] =
    for {
      db            <- ref.get
      addedAtMillis <- C.realTime(TimeUnit.MILLISECONDS)
      addedAt = Instant.ofEpochMilli(addedAtMillis)
      meta    = Schema.Metadata(addedAt, addedAt, isPublic)
      schema  = Schema(schemaMap, meta, body, None)
      _ <- ref.update(_.copy(schemas = db.schemas.updated(schemaMap, schema)))
      _ <- updateSupersedingVersion(schemaMap, supersedes.toSet)
    } yield ()

  def updateSchema(schemaMap: SchemaMap, body: Json, isPublic: Boolean)(
    implicit C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit] =
    addSchema(schemaMap, body, isPublic, List.empty)

  def getSchemas(implicit F: Bracket[F, Throwable]): F[List[Schema]] =
    ref
      .get
      .map(state =>
        state
          .schemas
          .values
          .toList
          .sortBy(schema => (schema.metadata.createdAt, schema.schemaMap.schemaKey.version))
          .map(addSupersedingInfo(state))
      )

  def getSchemasKeyOnly(implicit F: Bracket[F, Throwable]): F[List[(SchemaMap, Schema.Metadata)]] =
    ref.get.map(state => state.schemas.values.toList.sortBy(_.metadata.createdAt).map(s => (s.schemaMap, s.metadata)))

  def getDraft(draftId: DraftId)(implicit B: Bracket[F, Throwable]): F[Option[SchemaDraft]] =
    for { db <- ref.get } yield db.drafts.get(draftId)

  def addDraft(draftId: DraftId, body: Json, isPublic: Boolean)(
    implicit C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit] =
    for {
      db            <- ref.get
      addedAtMillis <- C.realTime(TimeUnit.MILLISECONDS)
      addedAt = Instant.ofEpochMilli(addedAtMillis)
      meta    = Schema.Metadata(addedAt, addedAt, isPublic)
      schema  = SchemaDraft(draftId, meta, body)
      _ <- ref.update(_.copy(drafts = db.drafts.updated(draftId, schema)))
    } yield ()

  def getDrafts(implicit F: Monad[F]): Stream[F, SchemaDraft] = {
    val drafts =
      ref.get.map(state => Stream.emits[F, SchemaDraft](state.drafts.values.toList.sortBy(_.metadata.createdAt)))
    Stream.eval(drafts).flatten
  }

  def addPermission(apikey: UUID, permission: Permission)(implicit F: Bracket[F, Throwable]): F[Unit] =
    for {
      db <- ref.get
      _  <- ref.update(_.copy(permission = db.permission.updated(apikey, permission)))
    } yield ()

  def deletePermission(apikey: UUID)(implicit F: Bracket[F, Throwable]): F[Unit] =
    for {
      db <- ref.get
      _  <- ref.update(_.copy(permission = db.permission - apikey))
    } yield ()

  def runAutomaticMigrations(implicit F: Bracket[F, Throwable]): F[Unit] =
    Bracket[F, Throwable].unit

  def addSupersedingInfo(db: InMemory.State)(schema: Schema): Schema =
    db.superseding.get(schema.schemaMap) match {
      case Some(version) => schema.copy(supersedingInfo = Some(SupersedingInfo.SupersededBy(version)))
      case None =>
        val vendor         = schema.schemaMap.schemaKey.vendor
        val name           = schema.schemaMap.schemaKey.name
        val currentVersion = schema.schemaMap.schemaKey.version
        val superseded = db
          .superseding
          .toList
          .collect {
            case (SchemaMap(SchemaKey(`vendor`, `name`, _, version)), `currentVersion`) => version
          }
          .sorted
        superseded match {
          case head :: tail => schema.copy(supersedingInfo = Some(SupersedingInfo.Supersedes(NonEmptyList(head, tail))))
          case _            => schema
        }
    }

  def updateSupersedingVersion(
    schemaMap: SchemaMap,
    supersedes: Set[SchemaVer.Full]
  )(implicit F: Bracket[F, Throwable]): F[Unit] =
    for {
      db <- ref.get
      transitive = db
        .superseding
        .toList
        .collect {
          case (schemaMap, version) if supersedes(version) => schemaMap.schemaKey.version
        }
        .toSet
      data = transitive.union(supersedes).foldLeft(db.superseding) {
        case (acc, version) =>
          val map = schemaMap.copy(schemaKey = schemaMap.schemaKey.copy(version = version))
          val newValue = Ordering[SchemaVer.Full].max(
            acc.get(map).getOrElse(schemaMap.schemaKey.version),
            schemaMap.schemaKey.version
          )
          acc.updated(map, newValue)
      }
      _ <- ref.update(_.copy(superseding = data))
    } yield ()
}

object InMemory {

  val DummySuperKey: UUID = UUID.fromString("48b267d7-cd2b-4f22-bae4-0f002008b5ad")

  case class State(
    schemas: Map[SchemaMap, Schema],
    superseding: Map[SchemaMap, SchemaVer.Full],
    permission: Map[UUID, Permission],
    drafts: Map[DraftId, SchemaDraft]
  )

  object State {
    val empty: State = State(Map.empty, Map.empty, Map.empty, Map.empty)

    /** Dev state */
    val withSuperKey: State =
      State(
        Map.empty[SchemaMap, Schema],
        Map.empty,
        Map(DummySuperKey -> Permission.Super),
        Map.empty[DraftId, SchemaDraft]
      )
  }

  def get[F[_]: Sync](fixture: State): F[Storage[F]] =
    for { db <- Ref.of(fixture) } yield InMemory[F](db)

  def empty[F[_]: Sync]: F[Storage[F]] =
    for { db <- Ref.of(State.withSuperKey) } yield InMemory[F](db)

  /** Equal to `get`, but doesn't lose the precise return type */
  def getInMemory[F[_]: Sync](fixture: State): F[InMemory[F]] =
    for { db <- Ref.of(fixture) } yield InMemory[F](db)
}
