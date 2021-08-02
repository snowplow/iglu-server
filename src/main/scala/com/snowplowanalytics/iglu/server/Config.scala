/*
 * Copyright (c) 2019-2021 Snowplow Analytics Ltd. All rights reserved.
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

import java.nio.file.Path

import cats.implicits._

import com.monovore.decline._

import io.circe.{Encoder, Json, JsonObject}
import io.circe.syntax._
import io.circe.generic.semiauto._

import pureconfig._
import pureconfig.generic.ProductHint
import pureconfig.generic.semiauto._
import pureconfig.module.http4s._
import migrations.MigrateFrom

import generated.BuildInfo.version

/**
  * Case class containing the Iglu Server's configuration options,
  * derived from a Typesafe Config configuration file.
  *
  * @param database       Database used by the server, either dummy (in-memory storage) or postgres instance
  * @param repoServer     Configuration options for the Iglu server - interface, port and deployment address
  * @param debug          If true, enables an additional endpoint (/api/debug) that outputs all internal state
  * @param patchesAllowed If true, schemas sent to the /api/schemas endpoint will overwrite existing ones rather than
  *                       be skipped if a schema with the same key already exists
  * @param webhooks       List of webhooks triggered by specific actions or endpoints
  */
case class Config(
  database: Config.StorageConfig,
  repoServer: Config.Http,
  debug: Option[Boolean],
  patchesAllowed: Option[Boolean],
  webhooks: Option[List[Webhook]]
)

object Config {

  sealed trait ThreadPool extends Product with Serializable
  object ThreadPool {
    case object Global          extends ThreadPool
    case object Cached          extends ThreadPool
    case class Fixed(size: Int) extends ThreadPool

    implicit val threadPoolReader: ConfigReader[ThreadPool] =
      ConfigReader.fromCursor { cur =>
        cur.asString match {
          case Right(s) if s.toLowerCase == "global" => Right(Global)
          case Right(s) if s.toLowerCase == "cached" => Right(Cached)
          case Left(err) =>
            for {
              obj     <- cur.asObjectCursor
              typeCur <- obj.atKey("type")
              typeStr <- typeCur.asString
              pool <- typeStr.toLowerCase match {
                case "fixed" =>
                  for {
                    sizeCur <- obj.atKey("size")
                    sizeInt <- sizeCur.asInt
                  } yield Fixed(sizeInt)
                case "global" => Right(Global)
                case "cached" => Right(Cached)
                case _        => Left(err)
              }
            } yield pool
        }
      }

    implicit val threadPoolCirceEncoder: Encoder[ThreadPool] =
      Encoder.instance {
        case Global =>
          Json.fromString("global")
        case Cached =>
          Json.fromString("cached")
        case Fixed(size) =>
          Json.fromFields(List(("size", Json.fromInt(size)), ("type", Json.fromString("fixed"))))
      }
  }

  sealed trait StorageConfig
  object StorageConfig {

    /** Frequently used HikariCP settings */
    sealed trait ConnectionPool extends Product with Serializable
    object ConnectionPool {
      case class NoPool(threadPool: ThreadPool = ThreadPool.Cached) extends ConnectionPool
      case class Hikari(
        connectionTimeout: Option[Int],
        maxLifetime: Option[Int],
        minimumIdle: Option[Int],
        maximumPoolSize: Option[Int],
        // Provided by doobie
        connectionPool: ThreadPool = ThreadPool.Fixed(2),
        transactionPool: ThreadPool = ThreadPool.Cached
      ) extends ConnectionPool

      implicit val circePoolEncoder: Encoder[ConnectionPool] =
        Encoder.instance {
          case NoPool(threadPool) =>
            Json.fromFields(
              List(
                ("type", Json.fromString("NoPool")),
                ("threadPool", threadPool.asJson(ThreadPool.threadPoolCirceEncoder))
              )
            )
          case h: Hikari =>
            deriveEncoder[Hikari].apply(h)
        }

      implicit val nopoolHint   = ProductHint[NoPool](ConfigFieldMapping(CamelCase, CamelCase))
      implicit val hikariHint   = ProductHint[Hikari](ConfigFieldMapping(CamelCase, CamelCase))
      implicit val noPoolReader = deriveReader[NoPool]
      implicit val hikariReader = deriveReader[Hikari]

      implicit val poolRead: ConfigReader[ConnectionPool] =
        ConfigReader.fromCursor { cur =>
          for {
            objCur <- cur.asObjectCursor
            typeCur = objCur.atKeyOrUndefined("type")
            pool <- if (typeCur.isUndefined) Right(ConfigReader[NoPool].from(cur).getOrElse(ConnectionPool.NoPool()))
            else
              for {
                typeStr <- typeCur.asString
                result <- typeStr.toLowerCase match {
                  case "hikari" =>
                    ConfigReader[Hikari].from(cur)
                  case "nopool" =>
                    ConfigReader[NoPool].from(cur)
                }
              } yield result
          } yield pool
        }
    }

    /**
      * Dummy in-memory configuration.
      */
    case object Dummy extends StorageConfig

    /**
      * Configuration for PostgreSQL state storage.
      */
    case class Postgres(
      host: String,
      port: Int,
      dbname: String,
      username: String,
      password: String,
      driver: String,
      connectThreads: Option[Int],
      maxPoolSize: Option[Int], // deprecated
      pool: ConnectionPool = ConnectionPool.NoPool(ThreadPool.Cached)
    ) extends StorageConfig {

      /** Backward-compatibility */
      val maximumPoolSize: Int = pool match {
        case ConnectionPool.NoPool(_) => 0
        case h: ConnectionPool.Hikari => h.maximumPoolSize.orElse(maxPoolSize).getOrElse(5)
      }
    }

    val postgresReader: ConfigReader[Postgres] =
      ConfigReader.forProduct9(
        "host",
        "port",
        "dbname",
        "username",
        "password",
        "driver",
        "connectThreads",
        "maxPoolSize",
        "pool"
      )(StorageConfig.Postgres.apply)

    implicit val storageConfigCirceEncoder: Encoder[StorageConfig] =
      deriveEncoder[StorageConfig].mapJson { json =>
        json.hcursor.downField("Postgres").focus.getOrElse(Json.Null).mapObject { o =>
          JsonObject.fromMap(o.toMap.map {
            case ("password", _) => ("password", Json.fromString("******"))
            case (k, v)          => (k, v)
          })
        }
      }
  }

  /**
    * Configuration options for the Iglu server.
    *
    * @param interface The server's host.
    * @param port The server's port.
    */
  case class Http(interface: String, port: Int, idleTimeout: Option[Int], threadPool: ThreadPool = ThreadPool.Fixed(4))

  implicit def httpConfigHint =
    ProductHint[Http](ConfigFieldMapping(CamelCase, CamelCase))

  implicit val httpConfigCirceEncoder: Encoder[Http] =
    deriveEncoder[Http]

  implicit val pureWebhookReader: ConfigReader[Webhook] = ConfigReader.fromCursor { cur =>
    for {
      objCur    <- cur.asObjectCursor
      uriCursor <- objCur.atKey("uri")
      uri       <- ConfigReader[org.http4s.Uri].from(uriCursor)

      prefixes <- objCur.atKeyOrUndefined("vendor-prefixes") match {
        case keyCur if keyCur.isUndefined => List.empty.asRight
        case keyCur                       => keyCur.asList.flatMap(_.traverse(cur => cur.asString))
      }
    } yield Webhook.SchemaPublished(uri, Some(prefixes))
  }

  implicit val pureStorageReader: ConfigReader[StorageConfig] = ConfigReader.fromCursor { cur =>
    for {
      objCur  <- cur.asObjectCursor
      typeCur <- objCur.atKey("type")
      typeStr <- typeCur.asString
      result <- typeStr match {
        case "postgres" => StorageConfig.postgresReader.from(cur)
        case "dummy"    => StorageConfig.Dummy.asRight
        case _ =>
          val message = s"type has value $typeStr instead of class1 or class2"
          objCur.failed[StorageConfig](error.CannotConvert(objCur.objValue.toString, "StorageConfig", message))
      }
    } yield result
  }

  implicit val pureHttpReader: ConfigReader[Http] = deriveReader[Http]

  implicit val pureWebhooksReader: ConfigReader[List[Webhook]] = ConfigReader.fromCursor { cur =>
    for {
      objCur                 <- cur.asObjectCursor
      schemaPublishedCursors <- objCur.atKeyOrUndefined("schema-published").asList
      webhooks               <- schemaPublishedCursors.traverse(cur => pureWebhookReader.from(cur))
    } yield webhooks
  }

  implicit val pureConfigReader: ConfigReader[Config] = deriveReader[Config]

  implicit val mainConfigCirceEncoder: Encoder[Config] =
    deriveEncoder[Config]

  sealed trait ServerCommand {
    def config: Path
    def read: Either[String, Config] =
      ConfigSource.default(ConfigSource.file(config)).load[Config].leftMap(_.toList.map(_.description).mkString("\n"))
  }

  object ServerCommand {
    case class Run(config: Path)                                 extends ServerCommand
    case class Setup(config: Path, migrate: Option[MigrateFrom]) extends ServerCommand
  }

  val configOpt = Opts.option[Path]("config", "Path to server configuration HOCON")
  val migrateOpt = Opts
    .option[String]("migrate", "Migrate the DB from a particular version")
    .mapValidated { s =>
      MigrateFrom.parse(s).toValid(s"Cannot perform migration from version $s to $version").toValidatedNel
    }
    .orNone

  val runCommand: Opts[ServerCommand] = configOpt.map(ServerCommand.Run.apply)
  val setupCommand: Opts[ServerCommand] =
    Opts.subcommand("setup", "Setup Iglu Server")((configOpt, migrateOpt).mapN(ServerCommand.Setup.apply))

  val serverCommand =
    Command[ServerCommand](generated.BuildInfo.name, generated.BuildInfo.version)(runCommand.orElse(setupCommand))
}
