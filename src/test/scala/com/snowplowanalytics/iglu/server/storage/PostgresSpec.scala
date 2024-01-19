/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.0
 * located at https://docs.snowplow.io/limited-use-license-1.0
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server
package storage

import java.util.UUID
import io.circe.Json
import cats.data.NonEmptyList
import cats.syntax.traverse._
import cats.instances.list._
import cats.effect.IO
import doobie._
import doobie.specs2._
import doobie.implicits._
import eu.timepit.refined.types.numeric.NonNegInt
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer}
import com.snowplowanalytics.iglu.server.model.{Permission, SchemaDraft}
import com.snowplowanalytics.iglu.server.migrations.Bootstrap

import scala.concurrent.ExecutionContext
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAll

class PostgresSpec extends Specification with BeforeAll with IOChecker {

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

  "Postgres speciication" should {
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
  }
}
