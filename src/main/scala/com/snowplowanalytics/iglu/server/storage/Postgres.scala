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
package com.snowplowanalytics.iglu.server
package storage

import java.util.UUID

import io.circe.Json

import cats.Monad
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.{Bracket, Clock}

import fs2.Stream

import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.postgres.circe.json.implicits._

import org.typelevel.log4cats.Logger

import com.snowplowanalytics.iglu.core.{SchemaMap, SchemaVer}
import com.snowplowanalytics.iglu.server.model.{Permission, Schema, SchemaDraft}
import com.snowplowanalytics.iglu.server.model.SchemaDraft.DraftId

class Postgres[F[_]](xa: Transactor[F], logger: Logger[F]) extends Storage[F] { self =>

  def getSchema(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Option[Schema]] =
    logger.debug(s"getSchemas ${schemaMap.schemaKey.toSchemaUri}") *>
      Postgres.Sql.getSchema(schemaMap).option.transact(xa)

  override def getSchemasByVendor(vendor: String)(implicit F: Bracket[F, Throwable]): Stream[F, Schema] =
    Stream.eval(logger.debug(s"getSchemasByVendor $vendor")) *>
      Postgres.Sql.getSchemasByVendor(vendor).stream.transact(xa)

  override def getSchemasByName(vendor: String, name: String)(
    implicit F: Bracket[F, Throwable]
  ): Stream[F, Schema] =
    Stream.eval(logger.debug(s"getSchemasByName $vendor/$name")) *>
      Postgres.Sql.getSchemasByName(vendor, name).stream.transact(xa)

  override def getSchemasByModel(vendor: String, name: String, model: Int)(
    implicit F: Bracket[F, Throwable]
  ): Stream[F, Schema] =
    Stream.eval(logger.debug(s"getSchemasByModel $vendor/$name/$model")) *>
      Postgres.Sql.getSchemasByModel(vendor, name, model).stream.transact(xa)

  def deleteSchema(schemaMap: SchemaMap)(implicit F: Bracket[F, Throwable]): F[Unit] =
    Postgres.Sql.deleteSchema(schemaMap).run.void.transact(xa)

  def getPermission(apikey: UUID)(implicit F: Bracket[F, Throwable]): F[Option[Permission]] =
    logger.debug(s"getPermission") *>
      Postgres.Sql.getPermission(apikey).option.transact(xa)

  def addSchema(schemaMap: SchemaMap, body: Json, isPublic: Boolean)(
    implicit C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit] =
    logger.debug(s"addSchema ${schemaMap.schemaKey.toSchemaUri}") *>
      Postgres.Sql.addSchema(schemaMap, body, isPublic).run.void.transact(xa)

  def updateSchema(schemaMap: SchemaMap, body: Json, isPublic: Boolean)(
    implicit C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit] =
    logger.debug(s"updateSchema ${schemaMap.schemaKey.toSchemaUri}") *>
      Postgres.Sql.updateSchema(schemaMap, body, isPublic).run.void.transact(xa)

  def getSchemas(implicit F: Bracket[F, Throwable]): F[List[Schema]] =
    logger.debug("getSchemas") *>
      Postgres.Sql.getSchemas.to[List].transact(xa)

  def getSchemasKeyOnly(implicit F: Bracket[F, Throwable]): F[List[(SchemaMap, Schema.Metadata)]] =
    logger.debug("getSchemasKeyOnly") *>
      Postgres.Sql.getSchemasKeyOnly.to[List].transact(xa)

  def getDraft(draftId: DraftId)(implicit B: Bracket[F, Throwable]): F[Option[SchemaDraft]] =
    Postgres.Sql.getDraft(draftId).option.transact(xa)

  def addDraft(draftId: DraftId, body: Json, isPublic: Boolean)(
    implicit C: Clock[F],
    M: Bracket[F, Throwable]
  ): F[Unit] =
    Postgres.Sql.addDraft(draftId, body, isPublic).run.void.transact(xa)

  def getDrafts(implicit F: Monad[F]): Stream[F, SchemaDraft] =
    Postgres.Sql.getDrafts.stream.transact(xa)

  def addPermission(uuid: UUID, permission: Permission)(implicit F: Bracket[F, Throwable]): F[Unit] =
    logger.debug(s"addPermission $permission") *>
      Postgres.Sql.addPermission(uuid, permission).run.void.transact(xa)

  def deletePermission(id: UUID)(implicit F: Bracket[F, Throwable]): F[Unit] =
    logger.debug(s"deletePermission") *>
      Postgres.Sql.deletePermission(id).run.void.transact(xa)

  def ping(implicit F: Bracket[F, Throwable]): F[Storage[F]] =
    logger.debug(s"ping") *>
      sql"SELECT 42".query[Int].unique.transact(xa).as(self)

  def drop(implicit F: Bracket[F, Throwable]): F[Unit] =
    logger.warn("Dropping database entities") *>
      (Postgres.Sql.dropSchemas.run.void *>
        Postgres.Sql.dropDrafts.run.void *>
        Postgres.Sql.dropPermissions.run.void *>
        Postgres.Sql.dropSchemaAction.run.void *>
        Postgres.Sql.dropKeyAction.run.void).transact(xa) *>
      logger.warn("Database entities were dropped")

  def addSupersededByColumn(implicit F: Bracket[F, Throwable]): F[Unit] =
    logger.debug(s"addSupersededByColumn") *>
      Postgres.Sql.addSupersededByColumn.run.void.transact(xa)

  def updateSupersedingVersion(
    vendor: String,
    name: String,
    superseded: NonEmptyList[SchemaVer.Full],
    supersededBy: SchemaVer.Full
  )(implicit F: Bracket[F, Throwable]): F[Unit] =
    logger.debug(s"updateSupersedingVersion: $vendor - $name - $superseded - $supersededBy") *>
      Postgres.Sql.updateSupersedingVersion(vendor, name, superseded, supersededBy).run.void.transact(xa)
}

object Postgres {

  val SchemasTable     = Fragment.const("iglu_schemas")
  val DraftsTable      = Fragment.const("iglu_drafts")
  val PermissionsTable = Fragment.const("iglu_permissions")

  val schemaColumns =
    Fragment.const(
      "vendor, name, format, model, revision, addition, created_at, updated_at, is_public, body, superseded_by"
    )
  val schemaKeyColumns =
    Fragment.const("vendor, name, format, model, revision, addition, created_at, updated_at, is_public")
  val draftColumns = Fragment.const("vendor, name, format, version, created_at, updated_at, is_public, body")

  val Ordering = Fragment.const("ORDER BY created_at")

  def apply[F[_]](xa: Transactor[F], logger: Logger[F]): Postgres[F] = new Postgres(xa, logger)

  def vendorFr(vendor: String): Fragment =
    fr"vendor = $vendor"

  def nameFr(vendor: String, name: String): Fragment =
    vendorFr(vendor) ++ fr"AND name = $name"

  def modelFr(vendor: String, name: String, model: Int): Fragment =
    nameFr(vendor, name) ++ fr"AND model = $model"

  def draftFr(id: DraftId): Fragment =
    nameFr(id.vendor, id.name) ++
      fr"AND format = ${id.format}" ++
      fr"AND version = ${id.version.value}"

  def schemaMapFr(schemaMap: SchemaMap): Fragment =
    nameFr(schemaMap.schemaKey.vendor, schemaMap.schemaKey.name) ++
      fr"AND format = ${schemaMap.schemaKey.format}" ++
      fr"AND" ++ schemaVerFr(schemaMap.schemaKey.version)

  def schemaVerFr(version: SchemaVer.Full): Fragment =
    fr"model = ${version.model} AND revision = ${version.revision} AND addition = ${version.addition}"

  def supersededByFr(version: SchemaVer.Full): Fragment =
    fr"superseded_by = ${version.asString}"

  def supersededFr(vendor: String, name: String, superseded: SchemaVer.Full): Fragment =
    fr"(" ++ nameFr(vendor, name) ++ fr"AND" ++ schemaVerFr(superseded) ++ fr")" ++
      fr"OR ${supersededByFr(superseded)}"

  def supersededListFr(vendor: String, name: String, supersededList: NonEmptyList[SchemaVer.Full]): Fragment =
    supersededList.map(supersededFr(vendor, name, _)).intercalate(fr"OR")

  object Sql {
    def getSchema(schemaMap: SchemaMap): Query0[Schema] =
      (fr"SELECT" ++ schemaColumns ++ fr"FROM" ++ SchemasTable ++ fr"WHERE" ++ schemaMapFr(schemaMap) ++ fr"LIMIT 1")
        .query[Schema]

    def getSchemasByName(vendor: String, name: String): Query0[Schema] =
      (fr"SELECT" ++ schemaColumns ++ fr"FROM" ++ SchemasTable ++ fr"WHERE" ++ nameFr(vendor, name) ++ Ordering)
        .query[Schema]

    def getSchemasByVendor(vendor: String): Query0[Schema] =
      (fr"SELECT" ++ schemaColumns ++ fr"FROM" ++ SchemasTable ++ fr"WHERE" ++ vendorFr(vendor) ++ Ordering)
        .query[Schema]

    def getSchemasByModel(vendor: String, name: String, model: Int): Query0[Schema] =
      (fr"SELECT" ++ schemaColumns ++ fr"FROM" ++ SchemasTable ++ fr"WHERE" ++ modelFr(vendor, name, model) ++ Ordering)
        .query[Schema]

    def deleteSchema(schemaMap: SchemaMap): Update0 =
      (fr"DELETE FROM" ++ SchemasTable ++ fr"WHERE" ++ schemaMapFr(schemaMap)).update

    def getSchemas: Query0[Schema] =
      (fr"SELECT" ++ schemaColumns ++ fr"FROM" ++ SchemasTable ++ Ordering).query[Schema]

    def getSchemasKeyOnly: Query0[(SchemaMap, Schema.Metadata)] =
      (fr"SELECT" ++ schemaKeyColumns ++ fr"FROM" ++ SchemasTable ++ Ordering).query[(SchemaMap, Schema.Metadata)]

    def addSchema(schemaMap: SchemaMap, schema: Json, isPublic: Boolean): Update0 = {
      val key = schemaMap.schemaKey
      val ver = key.version
      (fr"INSERT INTO" ++ SchemasTable ++ fr"(" ++ schemaColumns ++ fr")" ++
        fr"VALUES (${key.vendor}, ${key.name}, ${key.format}, ${ver.model}, ${ver.revision}, ${ver.addition}, current_timestamp, current_timestamp, $isPublic, $schema, null)").update
    }

    def updateSchema(schemaMap: SchemaMap, schema: Json, isPublic: Boolean): Update0 =
      (fr"UPDATE" ++ SchemasTable ++ fr"SET created_at = current_timestamp, updated_at = current_timestamp, is_public = $isPublic, body = $schema"
        ++ fr"WHERE" ++ schemaMapFr(schemaMap)).update

    def getDraft(draftId: DraftId): Query0[SchemaDraft] =
      (fr"SELECT" ++ draftColumns ++ fr"FROM" ++ DraftsTable ++ fr"WHERE " ++ draftFr(draftId)).query[SchemaDraft]

    def addDraft(id: DraftId, body: Json, isPublic: Boolean) =
      (fr"INSERT INTO" ++ DraftsTable ++ fr"(" ++ draftColumns ++ fr")" ++
        fr"VALUES (${id.vendor}, ${id.name}, ${id.format}, ${id.version.value}, current_timestamp, current_timestamp, $isPublic, $body)").update

    def getDrafts: Query0[SchemaDraft] =
      (fr"SELECT * FROM" ++ DraftsTable ++ Ordering).query[SchemaDraft]

    def getPermission(id: UUID) =
      (fr"SELECT vendor, wildcard, schema_action::schema_action, key_action::key_action[] FROM" ++ PermissionsTable ++ fr"WHERE apikey = $id")
        .query[Permission]

    def addPermission(uuid: UUID, permission: Permission): Update0 = {
      val vendor     = permission.vendor.parts.mkString(".")
      val keyActions = permission.key.toList.map(_.show)
      (fr"INSERT INTO" ++ PermissionsTable ++ sql"VALUES ($uuid, $vendor, ${permission.vendor.wildcard}, ${permission.schema}::schema_action, $keyActions::key_action[])").update
    }

    def deletePermission(id: UUID): Update0 =
      (fr"DELETE FROM" ++ PermissionsTable ++ fr"WHERE apikey = $id").update

    // Non-production statements

    def dropSchemas: Update0 =
      (fr"DROP TABLE IF EXISTS" ++ SchemasTable).update

    def dropDrafts: Update0 =
      (fr"DROP TABLE IF EXISTS" ++ DraftsTable).update

    def dropPermissions: Update0 =
      (fr"DROP TABLE IF EXISTS" ++ PermissionsTable).update

    def dropSchemaAction: Update0 =
      sql"DROP TYPE IF EXISTS schema_action".update

    def dropKeyAction: Update0 =
      sql"DROP TYPE IF EXISTS key_action".update

    def updateSupersedingVersion(
      vendor: String,
      name: String,
      superseded: NonEmptyList[SchemaVer.Full],
      supersededBy: SchemaVer.Full
    ): Update0 =
      (fr"UPDATE" ++ SchemasTable ++ fr"SET" ++ supersededByFr(supersededBy) ++ fr"WHERE" ++ supersededListFr(
        vendor,
        name,
        superseded
      )).update

    def addSupersededByColumn: Update0 =
      (fr"ALTER TABLE" ++ SchemasTable ++ fr"ADD COLUMN IF NOT EXISTS superseded_by VARCHAR(32)").update
  }
}
