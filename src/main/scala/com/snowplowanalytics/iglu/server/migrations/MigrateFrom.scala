/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.migrations

import doobie.free.connection.ConnectionIO

sealed trait MigrateFrom {
  def perform: ConnectionIO[Unit]
}

object MigrateFrom {
  case object `0.5.0` extends MigrateFrom {
    def perform: ConnectionIO[Unit] =
      Fifth.perform
  }

  def parse(s: String): Option[MigrateFrom] = s match {
    case "0.5.0" => Some(`0.5.0`)
    case _       => None
  }
}
