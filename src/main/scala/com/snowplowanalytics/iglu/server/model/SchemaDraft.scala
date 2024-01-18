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
package model

import doobie._
import doobie.postgres.circe.json.implicits._
import doobie.postgres.implicits._

import io.circe.{Encoder, Json}
import io.circe.generic.semiauto.deriveEncoder
import io.circe.refined._

import eu.timepit.refined.types.numeric.NonNegInt

import com.snowplowanalytics.iglu.server.model.SchemaDraft.DraftId
import storage.Storage.IncompatibleStorage

import Schema.Metadata

case class SchemaDraft(schemaMap: DraftId, metadata: Metadata, body: Json)

object SchemaDraft {
  case class DraftId(vendor: String, name: String, format: String, version: DraftVersion)

  implicit def draftIdEncoder: Encoder[DraftId] =
    deriveEncoder[DraftId]

  implicit def draftEncoder: Encoder[SchemaDraft] =
    deriveEncoder[SchemaDraft]

  implicit val draftVersionMeta: Meta[DraftVersion] =
    Meta[Int].timap(int => NonNegInt.from(int).fold(x => throw IncompatibleStorage(x), identity))(x => x.value)

  implicit val schemaDraftDoobieRead: Read[SchemaDraft] =
    Read[(DraftId, Metadata, Json)].map {
      case (id, meta, body) =>
        SchemaDraft(id, meta, body)
    }
}
