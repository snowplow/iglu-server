/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.0
 * located at https://docs.snowplow.io/limited-use-license-1.0
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
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
        supersedes VARCHAR(32) ARRAY NULL
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
