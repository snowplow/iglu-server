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
import sbt._

object Dependencies {

  object V {
    val IgluCore   = "1.1.2"
    val SchemaDdl  = "0.18.0"
    val IgluClient = "1.1.1"

    val Http4s     = "0.21.34"
    val Rho        = "0.21.0"
    val Doobie     = "0.13.4"
    val Decline    = "1.4.0"
    val Log4Cats   = "1.3.0"
    val Circe      = "0.14.3"
    val CirceFs2   = "0.13.0"
    val Refined    = "0.9.24"
    val PureConfig = "0.15.0"
    val SwaggerUi  = "4.15.5"
    val Slf4j      = "1.7.36"
    val ScalaCache = "0.28.0"
    val Postgresql = "42.5.5"
    val Jackson    = "2.14.1"
    val Snakeyaml  = "2.0"
    val Guava      = "32.0.0-jre"

    val Specs2     = "4.5.1"
    val Logback    = "1.2.3"
  }

  val all = Seq(
    "com.snowplowanalytics" %% "iglu-core-circe"       % V.IgluCore,
    "com.snowplowanalytics" %% "schema-ddl"            % V.SchemaDdl,
    "com.snowplowanalytics" %% "iglu-scala-client"     % V.IgluClient,

    "com.monovore"          %% "decline"               % V.Decline,
    "org.typelevel"         %% "log4cats-slf4j"        % V.Log4Cats,
    "org.http4s"            %% "http4s-blaze-server"   % V.Http4s,
    "org.http4s"            %% "http4s-blaze-client"   % V.Http4s,
    "org.http4s"            %% "http4s-circe"          % V.Http4s,
    "org.http4s"            %% "http4s-dsl"            % V.Http4s,
    "org.http4s"            %% "rho-swagger"           % V.Rho,
    "io.circe"              %% "circe-generic"         % V.Circe,
    "io.circe"              %% "circe-jawn"            % V.Circe,
    "io.circe"              %% "circe-literal"         % V.Circe,
    "io.circe"              %% "circe-refined"         % V.Circe,
    "io.circe"              %% "circe-fs2"             % V.CirceFs2,
    "eu.timepit"            %% "refined"               % V.Refined,
    "com.github.pureconfig" %% "pureconfig"            % V.PureConfig,
    "com.github.pureconfig" %% "pureconfig-http4s"     % V.PureConfig,
    "org.tpolecat"          %% "doobie-core"           % V.Doobie,
    "org.tpolecat"          %% "doobie-postgres"       % V.Doobie,
    "org.tpolecat"          %% "doobie-postgres-circe" % V.Doobie,
    "org.tpolecat"          %% "doobie-hikari"         % V.Doobie,
    "com.github.cb372"      %% "scalacache-cats-effect" % V.ScalaCache,
    "com.github.cb372"      %% "scalacache-caffeine"    % V.ScalaCache,

    "org.webjars"                % "swagger-ui"        % V.SwaggerUi,
    "org.slf4j"                  % "slf4j-simple"      % V.Slf4j,
    "org.postgresql"             % "postgresql"        % V.Postgresql,
    "com.fasterxml.jackson.core" % "jackson-databind"  % V.Jackson, // override transitive version to address security vulnerabilities
    "org.yaml"                   % "snakeyaml"         % V.Snakeyaml, // override transitive version to address security vulnerabilities
    "com.google.guava"           % "guava"             % V.Guava, // override transitive version to address security vulnerabilities


    "org.tpolecat"          %% "doobie-specs2"         % V.Doobie     % Test,
    "org.specs2"            %% "specs2-core"           % V.Specs2     % Test,
    "org.specs2"            %% "specs2-cats"           % V.Specs2     % Test
  )
}
