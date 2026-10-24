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
    // Snowplow libraries
    val IgluCore   = "1.1.6"
    val SchemaDdl  = "0.28.0"
    val IgluClient = "4.2.1"

    // Scala and FP core
    val Decline      = "2.6.2"
    val Log4Cats     = "2.8.0"
    val Circe        = "0.14.16"
    val CirceRefined = "0.15.1" // Released separately from circe since 0.14.10
    val Refined      = "0.11.4"
    val PureConfig   = "0.17.8"
    val Doobie       = "1.0.0-RC12"
    val Caffeine     = "3.3.0"

    // HTTP server and the Swagger API docs
    val Http4s      = "0.23.38"
    val Http4sBlaze = "0.23.18" // Released separately from http4s since 0.23.13
    val Tapir       = "1.13.32"
    val ApiSpec     = "0.11.10" // Aligned with tapir-openapi-docs
    val SwaggerUi   = "4.15.5"

    // Database driver
    val Postgresql = "42.7.14"

    // Logging
    val Slf4j = "2.0.20"

    // Overrides of transitive versions, to address security vulnerabilities
    val Jackson = "2.21.7"

    // Test only
    val Specs2            = "4.23.0" // Aligned with cats-effect-testing-specs2 and doobie-specs2
    val CatsEffectTesting = "1.8.0"
  }

  // Snowplow libraries
  val igluCoreCirce = "com.snowplowanalytics" %% "iglu-core-circe"   % V.IgluCore
  val schemaDdl     = "com.snowplowanalytics" %% "schema-ddl"        % V.SchemaDdl
  val igluClient    = "com.snowplowanalytics" %% "iglu-scala-client" % V.IgluClient

  // Scala and FP core
  val decline             = "com.monovore"                  %% "decline"               % V.Decline
  val log4cats            = "org.typelevel"                 %% "log4cats-slf4j"        % V.Log4Cats
  val circeGeneric        = "io.circe"                      %% "circe-generic"         % V.Circe
  val circeJawn           = "io.circe"                      %% "circe-jawn"            % V.Circe
  val circeLiteral        = "io.circe"                      %% "circe-literal"         % V.Circe
  val circeRefined        = "io.circe"                      %% "circe-refined"         % V.CirceRefined
  val refined             = "eu.timepit"                    %% "refined"               % V.Refined
  val pureconfig          = "com.github.pureconfig"         %% "pureconfig"            % V.PureConfig
  val pureconfigHttp4s    = "com.github.pureconfig"         %% "pureconfig-http4s"     % V.PureConfig
  val doobieCore          = "org.tpolecat"                  %% "doobie-core"           % V.Doobie
  val doobiePostgres      = "org.tpolecat"                  %% "doobie-postgres"       % V.Doobie
  val doobiePostgresCirce = "org.tpolecat"                  %% "doobie-postgres-circe" % V.Doobie
  val doobieHikari        = "org.tpolecat"                  %% "doobie-hikari"         % V.Doobie
  val caffeine            = "com.github.ben-manes.caffeine" % "caffeine"               % V.Caffeine

  // HTTP server and the Swagger API docs
  val http4sBlazeServer = "org.http4s"                    %% "http4s-blaze-server" % V.Http4sBlaze
  val http4sBlazeClient = "org.http4s"                    %% "http4s-blaze-client" % V.Http4sBlaze
  val http4sCirce       = "org.http4s"                    %% "http4s-circe"        % V.Http4s
  val http4sDsl         = "org.http4s"                    %% "http4s-dsl"          % V.Http4s
  val swaggerUi         = "org.webjars"                   % "swagger-ui"           % V.SwaggerUi
  val tapirCore         = "com.softwaremill.sttp.tapir"   %% "tapir-core"          % V.Tapir
  val tapirHttp4s       = "com.softwaremill.sttp.tapir"   %% "tapir-http4s-server" % V.Tapir
  val tapirCirce        = "com.softwaremill.sttp.tapir"   %% "tapir-json-circe"    % V.Tapir
  val tapirOpenApi      = "com.softwaremill.sttp.tapir"   %% "tapir-openapi-docs"  % V.Tapir
  val openApiCirce      = "com.softwaremill.sttp.apispec" %% "openapi-circe"       % V.ApiSpec

  // Database driver
  val postgresql = "org.postgresql" % "postgresql" % V.Postgresql

  // Logging
  val slf4jSimple = "org.slf4j" % "slf4j-simple" % V.Slf4j

  // Overrides of transitive versions, to address security vulnerabilities.
  // Each one names the direct dependencies that pull it in: after bumping those, drop the override
  // if the resolved runtime classpath is unchanged without it.
  val jacksonCore     = "com.fasterxml.jackson.core" % "jackson-core"     % V.Jackson // schema-ddl, iglu-scala-client
  val jacksonDatabind = "com.fasterxml.jackson.core" % "jackson-databind" % V.Jackson // schema-ddl, iglu-scala-client

  val securityOverrides = Seq(jacksonCore, jacksonDatabind)

  // Test only
  val doobieSpecs2      = "org.tpolecat"  %% "doobie-specs2"              % V.Doobie            % Test
  val specs2Core        = "org.specs2"    %% "specs2-core"                % V.Specs2            % Test
  val specs2Cats        = "org.specs2"    %% "specs2-cats"                % V.Specs2            % Test
  val catsEffectTesting = "org.typelevel" %% "cats-effect-testing-specs2" % V.CatsEffectTesting % Test

  val all = Seq(
    igluCoreCirce,
    schemaDdl,
    igluClient,
    decline,
    log4cats,
    http4sBlazeServer,
    http4sBlazeClient,
    http4sCirce,
    http4sDsl,
    tapirCore,
    tapirHttp4s,
    tapirCirce,
    tapirOpenApi,
    openApiCirce,
    circeGeneric,
    circeJawn,
    circeLiteral,
    circeRefined,
    refined,
    pureconfig,
    pureconfigHttp4s,
    doobieCore,
    doobiePostgres,
    doobiePostgresCirce,
    doobieHikari,
    caffeine,
    swaggerUi,
    slf4jSimple,
    postgresql,
    doobieSpecs2,
    specs2Core,
    specs2Cats,
    catsEffectTesting
  ) ++ securityOverrides
}
