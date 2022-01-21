/*
 * Copyright (c) 2019-2022 Snowplow Analytics Ltd. All rights reserved.
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
