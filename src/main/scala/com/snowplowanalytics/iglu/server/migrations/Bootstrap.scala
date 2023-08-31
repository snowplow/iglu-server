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
package com.snowplowanalytics.iglu.server.migrations

import cats.effect.Bracket
import cats.implicits._

import doobie._
import doobie.implicits._

import com.snowplowanalytics.iglu.server.storage.Postgres

object Bootstrap {
  val keyActionCreate =
    sql"""DO 'BEGIN
    PERFORM ''key_action''::regtype;
EXCEPTION
    WHEN undefined_object THEN
        CREATE TYPE key_action AS ENUM (''CREATE'', ''DELETE'');
END';"""

  val schemaActionCreate =
    sql"""DO 'BEGIN
    PERFORM ''schema_action''::regtype;
EXCEPTION
    WHEN undefined_object THEN
        CREATE TYPE schema_action AS ENUM (''READ'', ''BUMP'', ''CREATE'', ''CREATE_VENDOR'');
END';"""

  val permissionsCreate = (fr"CREATE TABLE IF NOT EXISTS" ++ Postgres.PermissionsTable ++ fr"""(
        apikey              UUID            NOT NULL,
        vendor              VARCHAR(128),
        wildcard            BOOL            NOT NULL,
        schema_action       schema_action,
        key_action          key_action[]    NOT NULL,
        PRIMARY KEY (apikey)
      );""")

  val schemasCreate = (fr"CREATE TABLE IF NOT EXISTS" ++ Postgres.SchemasTable ++ fr"""(
        vendor      VARCHAR(128)  NOT NULL,
        name        VARCHAR(128)  NOT NULL,
        format      VARCHAR(128)  NOT NULL,
        model       INTEGER       NOT NULL,
        revision    INTEGER       NOT NULL,
        addition    INTEGER       NOT NULL,

        created_at  TIMESTAMP     NOT NULL,
        updated_at  TIMESTAMP     NOT NULL,
        is_public   BOOLEAN       NOT NULL,

        body        JSON          NOT NULL,

        superseded_by VARCHAR(32) NULL,
        PRIMARY KEY(vendor, name, format, model, revision, addition)
      )""")

  val draftsCreate = (fr"CREATE TABLE IF NOT EXISTS" ++ Postgres.DraftsTable ++ fr"""(
        vendor      VARCHAR(128) NOT NULL,
        name        VARCHAR(128) NOT NULL,
        format      VARCHAR(128) NOT NULL,
        version     INTEGER      NOT NULL,

        created_at  TIMESTAMP    NOT NULL,
        updated_at  TIMESTAMP    NOT NULL,
        is_public   BOOLEAN      NOT NULL,

        body        JSON         NOT NULL
      )""")

  val allStatements =
    List(keyActionCreate, schemaActionCreate, permissionsCreate, schemasCreate, draftsCreate)

  def initialize[F[_]](xa: Transactor[F])(implicit F: Bracket[F, Throwable]) =
    allStatements
      .traverse[ConnectionIO, Int](sql => sql.updateWithLogHandler(LogHandler.jdkLogHandler).run)
      .map(_.combineAll)
      .transact(xa)
}
