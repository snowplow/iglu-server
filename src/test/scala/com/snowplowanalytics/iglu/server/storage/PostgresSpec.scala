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
import java.util.concurrent.atomic.AtomicInteger
import io.circe.Json
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.instances.list._
import cats.~>
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.effect.Deferred
import cats.syntax.parallel._
import doobie._
import doobie.specs2._
import doobie.implicits._
import eu.timepit.refined.types.numeric.NonNegInt
import org.typelevel.log4cats.slf4j.Slf4jLogger
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer}
import com.snowplowanalytics.iglu.server.model.{Permission, SchemaDraft}
import com.snowplowanalytics.iglu.server.migrations.Bootstrap

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAll

class PostgresSpec extends Specification with BeforeAll with IOChecker {

  // Examples share a single database; run them in declaration order so the table-mutating
  // ones cannot race the others.
  sequential

  val transactor = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost:5432/testdb",
    "postgres",
    "iglusecret",
    None
  )

  def beforeAll(): Unit = {
    val dropStatement =
      List(
        fr"DROP TABLE IF EXISTS" ++ Postgres.PermissionsTable,
        fr"DROP TABLE IF EXISTS" ++ Postgres.SchemasTable,
        fr"DROP TABLE IF EXISTS" ++ Postgres.DraftsTable,
        fr"DROP TYPE IF EXISTS schema_action",
        fr"DROP TYPE IF EXISTS key_action"
      ).map(_.update.run).sequence

    val action = dropStatement.transact(transactor) *>
      Bootstrap.initialize(transactor)

    action.unsafeRunSync()
    println(s"DB entities recreated")
  }

  val trivial = sql"SELECT 42".query[Int]

  /** Whether iglu_schemas and iglu_drafts have a primary key */
  def hasPrimaryKeys: ConnectionIO[(Boolean, Boolean)] =
    (
      Postgres.Sql.hasPrimaryKey(Postgres.SchemasTableName).unique,
      Postgres.Sql.hasPrimaryKey(Postgres.DraftsTableName).unique
    ).tupled

  /** Transaction failing with `sqlState` on its first `failures` attempts, then returning the attempt number */
  def failingTransaction(failures: Int, sqlState: String): (AtomicInteger, ConnectionIO[Int]) = {
    val attempts = new AtomicInteger(0)
    val transaction = FC.delay(attempts.incrementAndGet()).flatMap { attempt =>
      if (attempt <= failures) FC.raiseError[Int](new SQLException(s"failure $attempt", sqlState))
      else FC.pure(attempt)
    }
    (attempts, transaction)
  }

  "Postgres specification" should {
    "check connection" in {
      check(trivial)
    }

    "typecheck getSchema" in {
      check(Postgres.Sql.getSchema(SchemaMap("does", "not", "exist", SchemaVer.Full(1, 0, 0))))
    }

    "typecheck getSchemasByName" in {
      check(Postgres.Sql.getSchemasByName("does.not.exist", "myschema"))
    }

    "typecheck getSchemasByVendor" in {
      check(Postgres.Sql.getSchemasByVendor("does.not.exist"))
    }

    "typecheck getSchemasByModel" in {
      check(Postgres.Sql.getSchemasByModel("does.not.exist", "myschema", 1))
    }

    "typecheck upsertSchema with patches allowed" in {
      check(
        Postgres
          .Sql
          .upsertSchema(
            SchemaMap("does", "not", "exist", SchemaVer.Full(1, 0, 0)),
            Json.fromFields(List.empty),
            true,
            List(SchemaVer.Full(1, 0, 0)),
            patchesAllowed = true
          )
      )
    }

    "typecheck upsertSchema with patches disallowed" in {
      check(
        Postgres
          .Sql
          .upsertSchema(
            SchemaMap("does", "not", "exist", SchemaVer.Full(1, 0, 0)),
            Json.fromFields(List.empty),
            true,
            List(SchemaVer.Full(1, 0, 0)),
            patchesAllowed = false
          )
      )
    }

    "typecheck getDraft" in {
      check(Postgres.Sql.getDraft(SchemaDraft.DraftId("does", "not", "exist", NonNegInt(2))))
    }

    "typecheck getPermission" in {
      check(Postgres.Sql.getPermission(UUID.fromString("6907ba19-b6e0-4126-a931-dd236eec2736")))
    }

    "check getPermission read wildcard = false correctly" in {
      val permission = Permission(Permission.Vendor(List("com", "acme"), wildcard = false), None, Set.empty)
      val uuid       = UUID.fromString("6907ba19-b6e0-4126-a931-dd236eec2736")
      val addPerm: IO[Int] = Postgres
        .Sql
        .addPermission(
          uuid,
          permission
        )
        .run
        .transact(transactor)
      val readPerm: IO[Option[Permission]] =
        Postgres.Sql.getPermission(uuid).option.transact(transactor)
      val optPerm = (addPerm *> readPerm).unsafeRunSync()
      optPerm must beSome(permission)
    }

    "typecheck addPermission" in {
      check(
        Postgres
          .Sql
          .addPermission(
            UUID.fromString("6907ba19-b6e0-4126-a931-dd236eec2736"),
            Permission(Permission.Vendor(List("com", "acme"), false), None, Set.empty)
          )
      )
    }

    "typecheck deletePermission" in {
      check(Postgres.Sql.deletePermission(UUID.fromString("6907ba19-b6e0-4126-a931-dd236eec2736")))
    }

    "typecheck getDrafts" in {
      check(Postgres.Sql.getDrafts)
    }

    "typecheck updateSupersedingVersion" in {
      check(
        Postgres
          .Sql
          .updateSupersedingVersion(
            SchemaMap(SchemaKey("vendor", "name", "jsonschema", SchemaVer.Full(1, 0, 3))),
            NonEmptyList.of(SchemaVer.Full(1, 0, 0), SchemaVer.Full(1, 0, 2))
          )
      )
    }

    "typecheck addSchemasPrimaryKey" in {
      check(Postgres.Sql.addSchemasPrimaryKey)
    }

    "typecheck addDraftsPrimaryKey" in {
      check(Postgres.Sql.addDraftsPrimaryKey)
    }

    "bootstrap creates the tables with their primary key" in {
      hasPrimaryKeys.transact(transactor).unsafeRunSync() must beEqualTo((true, true))
    }

    "automatic migrations fail if a missing primary key cannot be added, and add it otherwise" in {
      val storage = Postgres(transactor, Slf4jLogger.getLogger[IO])
      // Plain INSERT: ON CONFLICT (hence upsertSchema) cannot be used on a table without primary key
      val insert = (fr"INSERT INTO" ++ Postgres.SchemasTable ++
        fr"""(vendor, name, format, model, revision, addition, created_at, updated_at, is_public, body)
        VALUES ('dup.vendor', 'dup', 'jsonschema', 1, 0, 0, current_timestamp, current_timestamp, true, '{}')""")
        .update
        .run

      // Tables as bootstrapped by a version predating their primary key, holding a duplicated schema
      val recreateLegacyTables =
        (fr"DROP TABLE IF EXISTS" ++ Postgres.SchemasTable).update.run *>
          (fr"DROP TABLE IF EXISTS" ++ Postgres.DraftsTable).update.run *>
          Bootstrap.schemasCreate.update.run *>
          Bootstrap.draftsCreate.update.run *>
          (fr"ALTER TABLE" ++ Postgres.SchemasTable ++ fr"DROP CONSTRAINT iglu_schemas_pkey").update.run *>
          (fr"ALTER TABLE" ++ Postgres.DraftsTable ++ fr"DROP CONSTRAINT iglu_drafts_pkey").update.run *>
          insert *>
          insert
      val removeDuplicate = (fr"DELETE FROM" ++ Postgres.SchemasTable).update.run *> insert

      val program = for {
        _           <- recreateLegacyTables.transact(transactor)
        failedRun   <- storage.runAutomaticMigrations.attempt
        pksAfterDup <- hasPrimaryKeys.transact(transactor)
        _           <- removeDuplicate.transact(transactor)
        _           <- storage.runAutomaticMigrations
        _           <- storage.runAutomaticMigrations
        pksAfterFix <- hasPrimaryKeys.transact(transactor)
      } yield (failedRun, pksAfterDup, pksAfterFix)

      val (failedRun, pksAfterDup, pksAfterFix) = program.unsafeRunSync()

      // 23505 = unique_violation, raised while building the index backing the primary key
      (failedRun must beLeft[Throwable].like { case e: java.sql.SQLException => e.getSQLState must beEqualTo("23505") })
        .and(pksAfterDup must beEqualTo((false, false)))
        .and(pksAfterFix must beEqualTo((true, true)))
    }

    "transactRetrying retries a transaction aborted with a transient error" in {
      val (attempts, transaction) = failingTransaction(failures = 2, Postgres.TransientError.DEADLOCK_DETECTED)

      val result = Postgres(transactor, Slf4jLogger.getLogger[IO]).transactRetrying(transaction).unsafeRunSync()

      (result must beEqualTo(3)).and(attempts.get must beEqualTo(3))
    }

    "transactRetrying gives up once the retries are exhausted" in {
      val (attempts, transaction) =
        failingTransaction(failures = Int.MaxValue, Postgres.TransientError.SERIALIZATION_FAILURE)

      val result = Postgres(transactor, Slf4jLogger.getLogger[IO]).transactRetrying(transaction).attempt.unsafeRunSync()

      (result must beLeft[Throwable].like {
        case e: SQLException => e.getSQLState must beEqualTo(Postgres.TransientError.SERIALIZATION_FAILURE)
      }).and(attempts.get must beEqualTo(1 + Postgres.TransientErrorRetries))
    }

    "transactRetrying does not retry other errors" in {
      val (attempts, transaction) = failingTransaction(failures = 1, "23505")

      val result = Postgres(transactor, Slf4jLogger.getLogger[IO]).transactRetrying(transaction).attempt.unsafeRunSync()

      (result must beLeft[Throwable].like { case e: SQLException => e.getSQLState must beEqualTo("23505") })
        .and(attempts.get must beEqualTo(1))
    }

    "transactRetrying recovers from a real deadlock" in {
      val storage = Postgres(transactor, Slf4jLogger.getLogger[IO])
      val table   = Fragment.const("iglu_deadlock_test")
      val setup = (fr"DROP TABLE IF EXISTS" ++ table).update.run *>
        (fr"CREATE TABLE" ++ table ++ fr"(id INT PRIMARY KEY, n INT NOT NULL)").update.run *>
        (fr"INSERT INTO" ++ table ++ fr"VALUES (1, 0), (2, 0)").update.run
      def increment(id: Int) = (fr"UPDATE" ++ table ++ fr"SET n = n + 1 WHERE id = $id").update.run

      // Each transaction locks one row, waits for the other to lock the second row, then tries to lock it:
      // Postgres aborts one of them with deadlock_detected, and the retry must succeed
      def transaction(
        first: Int,
        second: Int,
        locked: Deferred[IO, Unit],
        otherLocked: Deferred[IO, Unit],
        liftIO: IO ~> ConnectionIO
      ) =
        increment(first) *>
          liftIO(locked.complete(()) *> otherLocked.get) *>
          increment(second)

      val program = WeakAsync.liftK[IO, ConnectionIO].use { liftIO =>
        for {
          _            <- setup.transact(transactor)
          firstLocked  <- Deferred[IO, Unit]
          secondLocked <- Deferred[IO, Unit]
          attempts     <- IO(new AtomicInteger(0))
          counted = (t: ConnectionIO[Int]) => FC.delay(attempts.incrementAndGet()) *> t
          _ <- (
            storage.transactRetrying(counted(transaction(1, 2, firstLocked, secondLocked, liftIO))),
            storage.transactRetrying(counted(transaction(2, 1, secondLocked, firstLocked, liftIO)))
          ).parTupled
          counters <- (fr"SELECT n FROM" ++ table ++ fr"ORDER BY id").query[Int].to[List].transact(transactor)
          _        <- (fr"DROP TABLE" ++ table).update.run.transact(transactor)
        } yield (attempts.get, counters)
      }

      val (attempts, counters) = program.unsafeRunSync()

      // One retry, and both transactions eventually committed their two increments
      (attempts must beEqualTo(3)).and(counters must beEqualTo(List(2, 2)))
    }
  }
}
