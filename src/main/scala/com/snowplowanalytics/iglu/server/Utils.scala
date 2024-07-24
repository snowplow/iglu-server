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

import io.circe.Encoder
import io.circe.syntax._

import fs2.{Stream, text}

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer}

object Utils {

  def toSchemaMap(schemaKey: SchemaKey): SchemaMap =
    SchemaMap(schemaKey.vendor, schemaKey.name, schemaKey.format, schemaKey.version.asInstanceOf[SchemaVer.Full])

  def toBytes[F[_], A: Encoder](a: A): Stream[F, Byte] =
    Stream.emit(a.asJson.noSpaces).through(text.utf8Encode)

}
