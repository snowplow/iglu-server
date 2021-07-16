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

import java.nio.file.Paths
import java.util.UUID

import org.http4s.implicits._

import io.circe.syntax._
import io.circe.literal._

import scala.concurrent.duration.DurationLong

import cats.syntax.either._

class ConfigSpec extends org.specs2.Specification {
  def is = s2"""
  parse run command without file $e1
  parse run config from file $e2
  parse run config with dummy DB from file $e3
  asJson hides postgres password $e4
  parse minimal config with dummy DB from file $e5
  """

  def e1 = {
    val input    = "--config foo.hocon"
    val expected = Config.ServerCommand.Run(Some(Paths.get("foo.hocon")))
    val result   = Config.serverCommand.parse(input.split(" ").toList)
    result must beRight(expected)
  }

  def e2 = {
    val config     = getClass.getResource("/valid-pg-config.conf").toURI
    val configPath = Paths.get(config)
    val input      = s"--config $configPath"
    val pool = Config
      .StorageConfig
      .ConnectionPool
      .Hikari(
        Some(5000.millis),
        Some(1000.millis),
        Some(5),
        Some(3),
        Config.ThreadPool.Fixed(4),
        Config.ThreadPool.Cached
      )
    val expected = Config(
      Config
        .StorageConfig
        .Postgres(
          "postgres",
          5432,
          "igludb",
          "sp_user",
          "sp_password",
          "org.postgresql.Driver",
          Some(5),
          pool
        ),
      Config.Http("0.0.0.0", 8080, Some(10.seconds), None, Config.ThreadPool.Global),
      true,
      true,
      List(
        Webhook.SchemaPublished(uri"https://example.com/endpoint", Some(List.empty)),
        Webhook.SchemaPublished(uri"https://example2.com/endpoint", Some(List("com", "org.acme", "org.snowplow")))
      ),
      Config.Swagger("/custom/prefix"),
      None
    )
    val result = Config.serverCommand.parse(input.split(" ").toList).leftMap(_.toString).flatMap(_.read)
    result must beRight(expected)
  }

  def e3 = {
    val config     = getClass.getResource("/valid-dummy-config.conf").toURI
    val configPath = Paths.get(config)
    val input      = s"--config $configPath"
    val expected =
      Config(
        Config.StorageConfig.Dummy,
        Config.Http("0.0.0.0", 8080, None, None, Config.ThreadPool.Fixed(2)),
        true,
        false,
        Nil,
        Config.Swagger(""),
        None
      )
    val result = Config.serverCommand.parse(input.split(" ").toList).leftMap(_.toString).flatMap(_.read)
    result must beRight(expected)
  }

  def e4 = {
    val input = Config(
      Config
        .StorageConfig
        .Postgres(
          "postgres",
          5432,
          "igludb",
          "sp_user",
          "sp_password",
          "org.postgresql.Driver",
          Some(5),
          Config.StorageConfig.ConnectionPool.NoPool(Config.ThreadPool.Fixed(2))
        ),
      Config.Http("0.0.0.0", 8080, None, None, Config.ThreadPool.Global),
      true,
      true,
      List(
        Webhook.SchemaPublished(uri"https://example.com/endpoint", Some(List.empty)),
        Webhook.SchemaPublished(uri"https://example2.com/endpoint", Some(List("com", "org.acme", "org.snowplow")))
      ),
      Config.Swagger("/custom/prefix"),
      Some(UUID.fromString("a71aa7d9-6cde-40f7-84b1-046d65dedf9e"))
    )

    val expected = json"""{
      "database" : {
        "username" : "sp_user",
        "host" : "postgres",
        "dbname" : "igludb",
        "port" : 5432,
        "driver" : "org.postgresql.Driver",
        "maxPoolSize" : 5,
        "pool" : {
          "type" : "NoPool",
          "threadPool" : {
            "size" : 2,
            "type" : "fixed"
          }
        },
        "password" : "******"
      },
      "repoServer" : {
        "interface" : "0.0.0.0",
        "port" : 8080,
        "idleTimeout": null,
        "maxConnections": null,
        "threadPool": "global"
      },
      "debug" : true,
      "patchesAllowed" : true,
      "webhooks" : [
        {
          "SchemaPublished" : {
            "uri" : "https://example.com/endpoint",
            "vendorPrefixes" : [
            ]
          }
        },
        {
          "SchemaPublished" : {
            "uri" : "https://example2.com/endpoint",
            "vendorPrefixes" : [
              "com",
              "org.acme",
              "org.snowplow"
            ]
          }
        }
      ],
      "swagger": {
        "baseUrl": "/custom/prefix"
      },
      "superApiKey": "******"
    }"""

    input.asJson must beEqualTo(expected)
  }

  def e5 = {
    val config     = getClass.getResource("/valid-pg-minimal.conf").toURI
    val configPath = Paths.get(config)
    val input      = s"--config $configPath"
    val pool       = Config.StorageConfig.ConnectionPool.NoPool(Config.ThreadPool.Cached)
    val expected = Config(
      Config
        .StorageConfig
        .Postgres(
          "postgres",
          5432,
          "igludb",
          "sp_user",
          "sp_password",
          "org.postgresql.Driver",
          None,
          pool
        ),
      Config.Http("0.0.0.0", 8080, None, None, Config.ThreadPool.Fixed(4)),
      false,
      false,
      Nil,
      Config.Swagger(""),
      None
    )
    val result = Config.serverCommand.parse(input.split(" ").toList).leftMap(_.toString).flatMap(_.read)
    result must beRight(expected)

  }
}
