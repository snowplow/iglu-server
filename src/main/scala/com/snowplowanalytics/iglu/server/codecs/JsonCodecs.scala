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

import cats.effect.Concurrent

import io.circe.Json

import org.http4s.circe._

import com.snowplowanalytics.iglu.core.{SchemaKey, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.CirceIgluCodecs._
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Schema, SchemaDraft}
import com.snowplowanalytics.iglu.server.service.MetaService.ServerInfo

trait JsonCodecs {

  implicit def representationEntityDecoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityDecoder[F, List[Schema]]

  implicit def representationEntityEncoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityEncoder[F, Schema.Repr]

  implicit def representationListEntityEncoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityEncoder[F, List[Schema.Repr]]

  implicit def stringArrayEntityEncoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityEncoder[F, List[String]]

  implicit def schemaEntityEncoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityEncoder[F, Schema]

  implicit def igluResponseEntityEncoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityEncoder[F, IgluResponse]

  implicit def schemaListEntityEncoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityEncoder[F, List[Schema]]

  implicit def selfDescribingSchemaEntityEncoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityEncoder[F, SelfDescribingSchema[Json]]

  implicit def selfDescribingSchemaEntityDecoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityDecoder[F, SelfDescribingSchema[Json]]

  implicit def selfDescribingSchemaListEntityDecoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityDecoder[F, List[SelfDescribingSchema[Json]]]

  implicit def igluResponseEntityDecoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityDecoder[F, IgluResponse]

  implicit def draftHttpEncoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityEncoder[F, SchemaDraft]

  implicit def serverInfoEntityEcoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityEncoder[F, ServerInfo]

  implicit def schemaKeyListEntityDecoder[F[_]: Concurrent] =
    CirceEntityCodec.circeEntityDecoder[F, List[SchemaKey]]
}

object JsonCodecs extends JsonCodecs
