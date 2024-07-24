/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.codecs

import cats.effect.Sync

import fs2.text.utf8Encode
import fs2.Stream

import io.circe.Json
import io.circe.fs2.byteStreamParser

import org.http4s.circe._
import org.http4s.{Entity, EntityEncoder, Headers}

import com.snowplowanalytics.iglu.core.{SchemaKey, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.CirceIgluCodecs._
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Schema, SchemaDraft}
import com.snowplowanalytics.iglu.server.service.MetaService.ServerInfo

trait JsonCodecs {

  case class JsonArrayStream[F[_], A](nonArray: Stream[F, A])

  implicit def arrayStreamEncoder[F[_]: Sync, A: EntityEncoder[F, ?]] = {
    val W = implicitly[EntityEncoder[F, A]]
    new EntityEncoder[F, JsonArrayStream[F, A]] {
      override def toEntity(stream: JsonArrayStream[F, A]): Entity[F] = {
        val commaSeparated =
          stream.nonArray.flatMap(W.toEntity(_).body).through(byteStreamParser[F]).map(_.noSpaces).intersperse(",")
        val wrapped = Stream.emit("[") ++ commaSeparated ++ Stream.emit("]")
        Entity(wrapped.through(utf8Encode))
      }

      override def headers: Headers = W.headers
    }
  }

  implicit def representationEntityDecoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityDecoder[F, List[Schema]]

  implicit def representationEntityEncoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityEncoder[F, Schema.Repr]

  implicit def representationListEntityEncoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityEncoder[F, List[Schema.Repr]]

  implicit def stringArrayEntityEncoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityEncoder[F, List[String]]

  implicit def schemaEntityEncoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityEncoder[F, Schema]

  implicit def igluResponseEntityEncoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityEncoder[F, IgluResponse]

  implicit def schemaListEntityEncoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityEncoder[F, List[Schema]]

  implicit def selfDescribingSchemaEntityEncoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityEncoder[F, SelfDescribingSchema[Json]]

  implicit def selfDescribingSchemaEntityDecoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityDecoder[F, SelfDescribingSchema[Json]]

  implicit def selfDescribingSchemaListEntityDecoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityDecoder[F, List[SelfDescribingSchema[Json]]]

  implicit def igluResponseEntityDecoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityDecoder[F, IgluResponse]

  implicit def draftHttpEncoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityEncoder[F, SchemaDraft]

  implicit def serverInfoEntityEcoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityEncoder[F, ServerInfo]

  implicit def schemaKeyListEntityDecoder[F[_]: Sync] =
    CirceEntityCodec.circeEntityDecoder[F, List[SchemaKey]]
}

object JsonCodecs extends JsonCodecs
