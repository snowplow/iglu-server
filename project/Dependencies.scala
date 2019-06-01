/*
 * Copyright (c) 2019 Snowplow Analytics Ltd. All rights reserved.
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

  val ResolutionRepos = Seq(
    // Speed-up build
    "snowplow" at "https://snowplow.bintray.com/snowplow-maven"
  )

  object V {
    val IgluCore   = "0.5.1"
    val SchemaDdl  = "0.10.0-M7"
    val IgluClient = "0.6.1-M1"

    val Http4s     = "0.20.11"
    val Rho        = "0.19.0"
    val Doobie     = "0.7.0"
    val Decline    = "0.6.2"
    val Log4Cats   = "0.3.0"
    val Circe      = "0.11.1"
    val CirceFs2   = "0.11.0"
    val Refined    = "0.9.3"
    val PureConfig = "0.10.2"
    val SwaggerUi  = "3.22.0"
    val Slf4j      = "1.7.26"
    val ScalaCache = "0.27.0"

    val Specs2     = "4.5.1"
    val Logback    = "1.2.3"
  }

  val all = Seq(
    "com.snowplowanalytics" %% "iglu-core-circe"       % V.IgluCore,
    "com.snowplowanalytics" %% "schema-ddl"            % V.SchemaDdl,
    "com.snowplowanalytics" %% "iglu-scala-client"     % V.IgluClient,

    "com.monovore"          %% "decline"               % V.Decline,
    "io.chrisdavenport"     %% "log4cats-slf4j"        % V.Log4Cats,
    "org.http4s"            %% "http4s-blaze-server"   % V.Http4s,
    "org.http4s"            %% "http4s-blaze-client"   % V.Http4s,
    "org.http4s"            %% "http4s-circe"          % V.Http4s,
    "org.http4s"            %% "http4s-dsl"            % V.Http4s,
    "org.http4s"            %% "rho-swagger"           % V.Rho,
    "io.circe"              %% "circe-generic"         % V.Circe,
    "io.circe"              %% "circe-java8"           % V.Circe,
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

    "org.webjars"           %  "swagger-ui"            % V.SwaggerUi,
    "org.slf4j"             %  "slf4j-simple"          % V.Slf4j,
    "org.tpolecat"          %% "doobie-specs2"         % V.Doobie     % Test,
    "org.specs2"            %% "specs2-core"           % V.Specs2     % Test,
    "org.specs2"            %% "specs2-cats"           % V.Specs2     % Test
  )
}
