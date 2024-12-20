/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.service

import cats.implicits._
import cats.effect.{ContextShift, IO, Resource, Timer}
import com.snowplowanalytics.iglu.server.storage.{InMemory, Postgres, Storage}
import com.snowplowanalytics.iglu.server.{Config, SpecHelpers}
import org.http4s.{HttpApp, HttpRoutes, Request, Response, Status}
import org.http4s.client.Client

trait StorageAgnosticSpec {
  def storageResource: Resource[IO, Storage[IO]]

  val client: Client[IO] = Client.fromHttpApp(HttpApp[IO](r => Response[IO]().withEntity(r.body).pure[IO]))

  def sendRequestsGetResponse(
    createService: Storage[IO] => HttpRoutes[IO]
  )(requests: List[Request[IO]]): IO[Response[IO]] =
    storageResource.use { storage =>
      val service = createService(storage)
      for {
        responses <- requests.traverse(service.run).value
      } yield responses.flatMap(_.lastOption).getOrElse(Response(Status.NotFound))
    }

  def sendRequestsGetState[A](
    createService: Storage[IO] => HttpRoutes[IO]
  )(obtainState: Storage[IO] => IO[A])(requests: List[Request[IO]]): IO[(List[Response[IO]], A)] =
    storageResource.use { storage =>
      val service = createService(storage)
      for {
        responses <- requests.traverse(service.run).value
        state     <- obtainState(storage)
      } yield (responses.getOrElse(List.empty), state)
    }
}

trait InMemoryStorageSpec { self: StorageAgnosticSpec =>
  final def storageResource: Resource[IO, Storage[IO]] =
    Resource.eval(InMemory.getInMemory[IO](SpecHelpers.exampleState))
}

trait PostgresStorageSpec { self: StorageAgnosticSpec =>
  import scala.concurrent.ExecutionContext.global
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO]     = IO.timer(global)

  val dbPoolConfig = Config
    .StorageConfig
    .ConnectionPool
    .Hikari(None, None, None, None, Config.ThreadPool.Cached, Config.ThreadPool.Cached)

  val storageConfig =
    Config
      .StorageConfig
      .Postgres(
        "localhost",
        5432,
        "testdb",
        "postgres",
        "iglusecret",
        "org.postgresql.Driver",
        None,
        dbPoolConfig,
        true
      )

  final def storageResource: Resource[IO, Storage[IO]] =
    for {
      storage <- Storage.initialize[IO](storageConfig)
      _       <- Resource.eval(storage.asInstanceOf[Postgres[IO]].drop)
      _       <- Resource.eval(storage.asInstanceOf[Postgres[IO]].bootstrap)
      _ <- Resource.eval(
        SpecHelpers.exampleState.permission.toList.map { case (key, perm) => storage.addPermission(key, perm) }.sequence
      )
      _ <- Resource.eval(
        SpecHelpers
          .exampleState
          .schemas
          .values
          .toList
          .map(schema => storage.addSchema(schema.schemaMap, schema.body, schema.metadata.isPublic, List.empty))
          .sequence
      )
    } yield storage
}
