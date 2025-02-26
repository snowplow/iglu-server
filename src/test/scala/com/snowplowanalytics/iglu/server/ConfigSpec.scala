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

  val noHsts = Config.Hsts(enable = false, maxAge = 365.days)

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
          pool,
          false
        ),
      Config.Http("0.0.0.0", 8080, Some(10.seconds), None, Config.ThreadPool.Global, noHsts, 100000),
      true,
      true,
      List(
        Webhook.SchemaPublished(uri"https://example.com/endpoint", Some(List.empty), usePost = false),
        Webhook.SchemaPublished(
          uri"https://example2.com/endpoint",
          Some(List("com", "org.acme", "org.snowplow")),
          usePost = false
        ),
        Webhook.SchemaPublished(uri"https://example3.com/endpoint", Some(List.empty), usePost = true)
      ),
      Config.Swagger("/custom/prefix"),
      None,
      42.seconds,
      true,
      Config.License(false),
      40
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
        Config.Http("0.0.0.0", 8080, None, None, Config.ThreadPool.Fixed(2), Config.Hsts(true, 365.days), 5000),
        true,
        false,
        Nil,
        Config.Swagger(""),
        None,
        1.seconds,
        false,
        Config.License(true),
        50
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
          Config.StorageConfig.ConnectionPool.NoPool(Config.ThreadPool.Fixed(2)),
          true
        ),
      Config.Http("0.0.0.0", 8080, None, None, Config.ThreadPool.Global, noHsts, 3000),
      true,
      true,
      List(
        Webhook.SchemaPublished(uri"https://example.com/endpoint", Some(List.empty), usePost = false),
        Webhook.SchemaPublished(
          uri"https://example2.com/endpoint",
          Some(List("com", "org.acme", "org.snowplow")),
          usePost = false
        )
      ),
      Config.Swagger("/custom/prefix"),
      Some(UUID.fromString("a71aa7d9-6cde-40f7-84b1-046d65dedf9e")),
      10.seconds,
      true,
      Config.License(true),
      100
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
        "password" : "******",
        "enableStartupChecks": true
      },
      "repoServer" : {
        "interface" : "0.0.0.0",
        "port" : 8080,
        "idleTimeout": null,
        "maxConnections": null,
        "threadPool": "global",
        "hsts": {
          "enable": false,
          "maxAge": "365 days"
        },
        "maxPayloadSize": 3000
      },
      "debug" : true,
      "patchesAllowed" : true,
      "webhooks" : [
        {
          "SchemaPublished" : {
            "uri" : "https://example.com/endpoint",
            "vendorPrefixes" : [
            ],
            "usePost": false
          }
        },
        {
          "SchemaPublished" : {
            "uri" : "https://example2.com/endpoint",
            "vendorPrefixes" : [
              "com",
              "org.acme",
              "org.snowplow"
            ],
            "usePost": false
          }
        }
      ],
      "swagger": {
        "baseUrl": "/custom/prefix"
      },
      "superApiKey": "******",
      "preTerminationPeriod": "10 seconds",
      "preTerminationUnhealthy": true,
      "license": {
        "accept": true
      },
      "maxJsonDepth": 100
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
          pool,
          true
        ),
      Config.Http("0.0.0.0", 8080, None, None, Config.ThreadPool.Fixed(4), noHsts, 100000),
      false,
      false,
      Nil,
      Config.Swagger(""),
      None,
      1.seconds,
      false,
      Config.License(false),
      40
    )
    val result = Config.serverCommand.parse(input.split(" ").toList).leftMap(_.toString).flatMap(_.read)
    result must beRight(expected)

  }
}
