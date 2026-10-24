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

import java.sql.SQLException
import java.util.UUID
import io.circe.Json
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.Async
import fs2.Stream
import doobie._
import doobie.implicits._
import doobie.postgres.implicits.{JavaInstantMeta => _, _}
import doobie.postgres.circe.json.implicits._
import org.typelevel.log4cats.Logger
import com.snowplowanalytics.iglu.core.{SchemaMap, SchemaVer}
import com.snowplowanalytics.iglu.server.migrations.Bootstrap
import com.snowplowanalytics.iglu.server.model.{Permission, Schema, SchemaDraft}
import com.snowplowanalytics.iglu.server.model.SchemaDraft.DraftId
import com.snowplowanalytics.iglu.server.model.Schema.Metadata.doobieInstantMeta

class Postgres[F[_]: Async](xa: Transactor[F], logger: Logger[F]) extends Storage[F] { self =>

  def getSchema(schemaMap: SchemaMap): F[Option[Schema]] =
    logger.debug(s"getSchemas ${schemaMap.schemaKey.toSchemaUri}") *>
      Postgres.Sql.getSchema(schemaMap).option.transact(xa)

  override def getSchemasByVendor(vendor: String): Stream[F, Schema] =
    Stream.eval(logger.debug(s"getSchemasByVendor $vendor")) *>
      Postgres.Sql.getSchemasByVendor(vendor).stream.transact(xa)

  override def getSchemasByName(vendor: String, name: String): Stream[F, Schema] =
    Stream.eval(logger.debug(s"getSchemasByName $vendor/$name")) *>
      Postgres.Sql.getSchemasByName(vendor, name).stream.transact(xa)

  override def getSchemasByModel(vendor: String, name: String, model: Int): Stream[F, Schema] =
    Stream.eval(logger.debug(s"getSchemasByModel $vendor/$name/$model")) *>
      Postgres.Sql.getSchemasByModel(vendor, name, model).stream.transact(xa)

  def deleteSchema(schemaMap: SchemaMap): F[Unit] =
    Postgres.Sql.deleteSchema(schemaMap).run.void.transact(xa)

  def getPermission(apikey: UUID): F[Option[Permission]] =
    logger.debug(s"getPermission") *>
      Postgres.Sql.getPermission(apikey).option.transact(xa)

  def upsertSchema(
    schemaMap: SchemaMap,
    body: Json,
    isPublic: Boolean,
    supersedes: List[SchemaVer.Full],
    patchesAllowed: Boolean
  ): F[Boolean] =
    logger.debug(s"upsertSchema ${schemaMap.schemaKey.toSchemaUri}") *>
      transactRetrying(for {
        wasInserted <- Postgres
          .Sql
          .upsertSchema(schemaMap, body, isPublic, supersedes, patchesAllowed)
          .option
          .map(_.getOrElse(false))
        // supersedes is only recorded for freshly inserted schemas
        _ <- supersedes match {
          case head :: tail if wasInserted =>
            Postgres.Sql.updateSupersedingVersion(schemaMap, NonEmptyList(head, tail)).run.void
          case _ => doobie.free.connection.unit
        }
      } yield wasInserted)

  def getSchemas: F[List[Schema]] =
    logger.debug("getSchemas") *>
      Postgres.Sql.getSchemas.to[List].transact(xa)

  def getSchemasKeyOnly: F[List[(SchemaMap, Schema.Metadata)]] =
    logger.debug("getSchemasKeyOnly") *>
      Postgres.Sql.getSchemasKeyOnly.to[List].transact(xa)

  def getDraft(draftId: DraftId): F[Option[SchemaDraft]] =
    Postgres.Sql.getDraft(draftId).option.transact(xa)

  def addDraft(draftId: DraftId, body: Json, isPublic: Boolean): F[Unit] =
    Postgres.Sql.addDraft(draftId, body, isPublic).run.void.transact(xa)

  def getDrafts: Stream[F, SchemaDraft] =
    Postgres.Sql.getDrafts.stream.transact(xa)

  def addPermission(uuid: UUID, permission: Permission): F[Unit] =
    logger.debug(s"addPermission $permission") *>
      Postgres.Sql.addPermission(uuid, permission).run.void.transact(xa)

  def deletePermission(id: UUID): F[Unit] =
    logger.debug(s"deletePermission") *>
      Postgres.Sql.deletePermission(id).run.void.transact(xa)

  def ping: F[Storage[F]] =
    logger.debug(s"ping") *>
      sql"SELECT 42".query[Int].unique.transact(xa).as(self)

  def drop: F[Unit] =
    logger.warn("Dropping database entities") *>
      (Postgres.Sql.dropSchemas.run.void *>
        Postgres.Sql.dropDrafts.run.void *>
        Postgres.Sql.dropPermissions.run.void *>
        Postgres.Sql.dropSchemaAction.run.void *>
        Postgres.Sql.dropKeyAction.run.void).transact(xa) *>
      logger.warn("Database entities were dropped")

  def runAutomaticMigrations: F[Unit] =
    logger.debug(s"Running automatic migrations") *>
      (Postgres.Sql.addSupersededByColumn.run.void *>
        Postgres.Sql.addSupersedesColumn.run.void).transact(xa) *>
      ensurePrimaryKey(Postgres.SchemasTableName, Postgres.Sql.addSchemasPrimaryKey) *>
      ensurePrimaryKey(Postgres.DraftsTableName, Postgres.Sql.addDraftsPrimaryKey) *>
      logger.debug(s"Automatic migrations done")

  /**
    * Create the primary key on tables bootstrapped before it was part of their definition.
    * The primary key is mandatory, so any failure to create it (e.g. duplicate rows) is fatal.
    */
  private def ensurePrimaryKey(table: String, addPrimaryKey: Update0): F[Unit] =
    Postgres.Sql.hasPrimaryKey(table).unique.transact(xa).flatMap {
      case true =>
        Async[F].unit
      case false =>
        logger.warn(s"Table $table has no primary key, adding it") *>
          addPrimaryKey.run.void.transact(xa).onError {
            case e => logger.error(e)(s"Could not add the primary key to table $table")
          } *>
          logger.info(s"Primary key added to table $table")
    }

  def bootstrap: F[Int] = Bootstrap.initialize(xa)

  /**
    * Run a transaction, running it again if Postgres aborted it with a transient error.
    * An aborted transaction is entirely rolled back, so it is safe to retry it as is.
    */
  private[storage] def transactRetrying[A](
    transaction: ConnectionIO[A],
    retries: Int = Postgres.TransientErrorRetries
  ): F[A] =
    transaction.transact(xa).handleErrorWith {
      case Postgres.TransientError(e) if retries > 0 =>
        logger
          .warn(s"Transaction aborted with transient error (SQLState ${e.getSQLState}), retrying: ${e.getMessage}") *>
          transactRetrying(transaction, retries - 1)
      case e =>
        Async[F].raiseError(e)
    }
}

object Postgres {
  val SchemasTableName = "iglu_schemas"
  val DraftsTableName  = "iglu_drafts"

  val SchemasTable     = Fragment.const(SchemasTableName)
  val DraftsTable      = Fragment.const(DraftsTableName)
  val PermissionsTable = Fragment.const("iglu_permissions")

  val schemaColumns =
    Fragment.const(
      "vendor, name, format, model, revision, addition, created_at, updated_at, is_public, body, superseded_by, supersedes"
    )
  val schemaKeyColumns =
    Fragment.const("vendor, name, format, model, revision, addition, created_at, updated_at, is_public")
  val draftColumns = Fragment.const("vendor, name, format, version, created_at, updated_at, is_public, body")

  val Ordering = Fragment.const("ORDER BY created_at")

  def apply[F[_]: Async](xa: Transactor[F], logger: Logger[F]): Postgres[F] = new Postgres(xa, logger)

  /** How many times a transaction aborted with a transient error is retried */
  val TransientErrorRetries = 3

  /** Errors after which the aborted transaction can be retried, by SQLState */
  object TransientError {
    val SERIALIZATION_FAILURE = "40001"
    val DEADLOCK_DETECTED     = "40P01"

    def unapply(error: Throwable): Option[SQLException] =
      error match {
        case e: SQLException =>
          e.getSQLState match {
            case SERIALIZATION_FAILURE | DEADLOCK_DETECTED => Some(e)
            case _                                         => None
          }
        case _ => None
      }
  }

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

  def supersededListFr(vendor: String, name: String, supersededList: NonEmptyList[SchemaVer.Full]): Fragment =
    Fragments.and(
      nameFr(vendor, name),
      supersededList
        .map(version =>
          Fragments.or(
            schemaVerFr(version),
            supersededByFr(version)
          )
        )
        .intercalate(fr"OR")
    )

  def supersededByInferiorFr(supersedingVersion: SchemaVer.Full) =
    Fragments.or(
      fr"superseded_by IS NULL",
      fr"STRING_TO_ARRAY(superseded_by, '-')::int[] < STRING_TO_ARRAY(${supersedingVersion.asString}, '-')::int[]"
    )

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

    /**
      * Atomic insert-or-update keyed on the schemas primary key. Returns a single boolean row that is
      * `true` when a new row was inserted and `false` when the row already existed:
      *   - `patchesAllowed`  -> `DO UPDATE` always yields a row; `xmax = 0` distinguishes insert from update
      *   - `!patchesAllowed` -> `DO NOTHING` yields a row only on insert; a conflict returns no row at all
      */
    def upsertSchema(
      schemaMap: SchemaMap,
      schema: Json,
      isPublic: Boolean,
      supersedes: List[SchemaVer.Full],
      patchesAllowed: Boolean
    ): Query0[Boolean] = {
      val key = schemaMap.schemaKey
      val ver = key.version
      val insertFr = fr"INSERT INTO" ++ SchemasTable ++ fr"(" ++ schemaColumns ++ fr")" ++
        fr"""VALUES (
          ${key.vendor}, ${key.name}, ${key.format},
          ${ver.model}, ${ver.revision}, ${ver.addition},
          current_timestamp, current_timestamp,
          $isPublic, $schema, null, ${supersedes.map(_.asString)})"""
      val conflictFr =
        if (patchesAllowed)
          fr"""ON CONFLICT (vendor, name, format, model, revision, addition) DO UPDATE SET
            updated_at = current_timestamp,
            is_public = EXCLUDED.is_public,
            body = EXCLUDED.body
          RETURNING (xmax = 0)"""
        else
          fr"ON CONFLICT (vendor, name, format, model, revision, addition) DO NOTHING RETURNING true"
      (insertFr ++ conflictFr).query[Boolean]
    }

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
      val keyActions = permission.key.toList
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
      schemaMap: SchemaMap,
      supersedes: NonEmptyList[SchemaVer.Full]
    ): Update0 =
      sql"""
        UPDATE $SchemasTable
        SET superseded_by = ${schemaMap.schemaKey.version.asString}
        WHERE ${Fragments.and(
        supersededByInferiorFr(schemaMap.schemaKey.version),
        supersededListFr(
          schemaMap.schemaKey.vendor,
          schemaMap.schemaKey.name,
          supersedes
        )
      )}""".update

    def addSupersededByColumn: Update0 =
      (fr"ALTER TABLE" ++ SchemasTable ++ fr"ADD COLUMN IF NOT EXISTS superseded_by VARCHAR(32)").update

    def addSupersedesColumn: Update0 =
      (fr"ALTER TABLE" ++ SchemasTable ++ fr"ADD COLUMN IF NOT EXISTS supersedes VARCHAR(32) ARRAY").update

    def hasPrimaryKey(table: String): Query0[Boolean] =
      sql"SELECT EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid = $table::regclass AND contype = 'p')"
        .query[Boolean]

    def addSchemasPrimaryKey: Update0 =
      (fr"ALTER TABLE" ++ SchemasTable ++ fr"ADD PRIMARY KEY (vendor, name, format, model, revision, addition)").update

    def addDraftsPrimaryKey: Update0 =
      (fr"ALTER TABLE" ++ DraftsTable ++ fr"ADD PRIMARY KEY (vendor, name, format, version)").update
  }
}
