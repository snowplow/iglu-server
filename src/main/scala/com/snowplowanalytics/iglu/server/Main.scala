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

import cats.data.EitherT
import cats.effect._
import cats.syntax.either._

object Main extends IOApp {

  def run(args: List[String]) = {
    val cli = for {
      command <- EitherT.fromEither[IO](Config.serverCommand.parse(args).leftMap(_.toString))
      config  <- EitherT.fromEither[IO](command.read)
      result <- command match {
        case _: Config.ServerCommand.Run =>
          EitherT.liftF[IO, String, ExitCode](Server.run(config))
        case Config.ServerCommand.Setup(_, migration) =>
          EitherT.liftF[IO, String, ExitCode](Server.setup(config, migration))
      }
    } yield result

    cli.value.flatMap {
      case Right(code)    => IO.pure(code)
      case Left(cliError) => IO(System.err.println(cliError)).as(ExitCode.Error)
    }
  }
}
