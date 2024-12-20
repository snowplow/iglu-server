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

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

import cats.implicits._
import cats.effect.Sync

import fs2.{Stream, text}

import org.http4s.{DecodeResult, EntityDecoder, InvalidMessageBodyFailure}
import org.http4s.circe.jsonDecoder

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer}

object Utils {

  def toSchemaMap(schemaKey: SchemaKey): SchemaMap =
    SchemaMap(schemaKey.vendor, schemaKey.name, schemaKey.format, schemaKey.version.asInstanceOf[SchemaVer.Full])

  def toBytes[F[_], A: Encoder](a: A): Stream[F, Byte] =
    Stream.emit(a.asJson.noSpaces).through(text.utf8Encode)

  def jsonOfWithDepthCheck[F[_]: Sync, A: Decoder](maxJsonDepth: Int): EntityDecoder[F, A] =
    jsonDecoderWithDepthCheck(maxJsonDepth).flatMapR { json =>
      json
        .as[A]
        .fold(
          failure =>
            DecodeResult.failureT[F, A](
              InvalidMessageBodyFailure(s"Could not decode JSON body", Some(failure))
            ),
          DecodeResult.successT[F, A](_)
        )
    }

  def jsonDecoderWithDepthCheck[F[_]: Sync](maxJsonDepth: Int): EntityDecoder[F, Json] =
    jsonDecoder[F].transform(
      _.flatMap { json =>
        if (checkIfExceedMaxDepth(json, maxJsonDepth))
          InvalidMessageBodyFailure("Maximum allowed JSON depth exceeded").asLeft
        else json.asRight
      }
    )

  private def checkIfExceedMaxDepth(json: Json, maxJsonDepth: Int): Boolean =
    if (maxJsonDepth <= 0) true
    else
      json.fold(
        jsonNull = false,
        jsonBoolean = _ => false,
        jsonNumber = _ => false,
        jsonString = _ => false,
        jsonArray = _.exists(checkIfExceedMaxDepth(_, maxJsonDepth - 1)),
        jsonObject = _.toList.exists { case (_, j) => checkIfExceedMaxDepth(j, maxJsonDepth - 1) }
      )
}
