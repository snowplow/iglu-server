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
import io.circe.Json
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.instances.list._
import cats.effect.IO
import doobie._
import doobie.specs2._
import doobie.implicits._
import eu.timepit.refined.types.numeric.NonNegInt
import org.typelevel.log4cats.slf4j.Slf4jLogger
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer}
import com.snowplowanalytics.iglu.server.model.{Permission, SchemaDraft}
import com.snowplowanalytics.iglu.server.migrations.Bootstrap

import scala.concurrent.ExecutionContext
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAll

class PostgresSpec extends Specification with BeforeAll with IOChecker {

  // Examples share a single database; run them in declaration order so the table-mutating
  // ones cannot race the others.
  sequential

  implicit val cs = IO.contextShift(ExecutionContext.global)

  val transactor = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost:5432/testdb",
    "postgres",
    "iglusecret"
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

    "typecheck addSchema" in {
      check(
        Postgres
          .Sql
          .addSchema(
            SchemaMap("does", "not", "exist", SchemaVer.Full(1, 0, 0)),
            Json.fromFields(List.empty),
            true,
            List(SchemaVer.Full(1, 0, 0))
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

    // hasPrimaryKey is not statically checkable: doobie's analysis rejects the text -> regclass
    // parameter cast even though it executes correctly (exercised by the migration test below).

    "typecheck addSchemasPrimaryKey" in {
      check(Postgres.Sql.addSchemasPrimaryKey)
    }

    "typecheck addDraftsPrimaryKey" in {
      check(Postgres.Sql.addDraftsPrimaryKey)
    }

    "typecheck duplicateSchemas" in {
      check(Postgres.Sql.duplicateSchemas)
    }

    "typecheck duplicateDrafts" in {
      check(Postgres.Sql.duplicateDrafts)
    }

    // Self-contained: recreates iglu_schemas without a PK so it does not depend on the
    // ordering of the other examples that share this database.
    "the primary key migration tolerates duplicates, reports them, and succeeds once they are gone" in {
      val storage = Postgres(transactor, Slf4jLogger.getLogger[IO])
      val map     = SchemaMap(SchemaKey("dup.vendor", "dup", "jsonschema", SchemaVer.Full(1, 0, 0)))
      val insert  = Postgres.Sql.addSchema(map, Json.fromFields(List.empty), isPublic = true, List.empty).run

      val recreateSchemas =
        (fr"DROP TABLE IF EXISTS" ++ Postgres.SchemasTable).update.run *>
          Bootstrap.schemasCreate.update.run

      val program = for {
        // Start from a schemas table without a primary key, holding a duplicate key tuple
        _             <- (recreateSchemas *> insert *> insert).transact(transactor)
        duplicateRows <- Postgres.Sql.duplicateSchemas.to[List].transact(transactor)
        // Migration must not raise even though the PK cannot be created yet
        _          <- storage.runAutomaticMigrations
        pkAfterDup <- Postgres.Sql.hasPrimaryKey(Postgres.SchemasTableName).unique.transact(transactor)
        // Remove the duplicate, then the migration must succeed and be idempotent on re-run
        _ <- (fr"DELETE FROM" ++ Postgres.SchemasTable ++ fr"WHERE ctid NOT IN (SELECT MIN(ctid) FROM" ++ Postgres.SchemasTable ++ fr"GROUP BY vendor, name, format, model, revision, addition)")
          .update
          .run
          .transact(transactor)
        _          <- storage.runAutomaticMigrations
        _          <- storage.runAutomaticMigrations
        pkAfterFix <- Postgres.Sql.hasPrimaryKey(Postgres.SchemasTableName).unique.transact(transactor)
        draftsPk   <- Postgres.Sql.hasPrimaryKey(Postgres.DraftsTableName).unique.transact(transactor)
      } yield (duplicateRows, pkAfterDup, pkAfterFix, draftsPk)

      val (duplicateRows, pkAfterDup, pkAfterFix, draftsPk) = program.unsafeRunSync()

      // Both whole rows are reported, each rendered as a text tuple containing the key fields
      duplicateRows must haveSize(2)
      forall(duplicateRows)(_ must contain("dup.vendor"))
      pkAfterDup must beFalse
      pkAfterFix must beTrue
      draftsPk must beTrue
    }
  }
}
