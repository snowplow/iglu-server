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

import io.circe.{Decoder, Encoder, Json, ParsingFailure}
import io.circe.syntax._

import cats.implicits._
import cats.effect.Concurrent

import fs2.{Stream, text}

import org.http4s.{DecodeFailure, DecodeResult, EntityDecoder, InvalidMessageBodyFailure}
import org.http4s.circe.jsonDecoder

import org.typelevel.jawn.MaxDepthExceededException

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer}

object Utils {

  def toSchemaMap(schemaKey: SchemaKey): SchemaMap =
    SchemaMap(schemaKey.vendor, schemaKey.name, schemaKey.format, schemaKey.version.asInstanceOf[SchemaVer.Full])

  def toBytes[F[_], A: Encoder](a: A): Stream[F, Byte] =
    Stream.emit(a.asJson.noSpaces).through(text.utf8.encode)

  def jsonOfWithDepthCheck[F[_]: Concurrent, A: Decoder](maxJsonDepth: Int): EntityDecoder[F, A] =
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

  /**
    * JSON nested deeper than `maxJsonDepth` is invalid (422). JSON nested deeper than the parser allows (4096 levels
    * for jawn) is too, rather than malformed (400), as it was before jawn had a depth limit.
    */
  def jsonDecoderWithDepthCheck[F[_]: Concurrent](maxJsonDepth: Int): EntityDecoder[F, Json] =
    jsonDecoder[F].transform(
      _.leftMap(failure => if (isTooDeep(failure)) tooDeep else failure).flatMap { json =>
        if (checkIfExceedMaxDepth(json, maxJsonDepth)) tooDeep.asLeft
        else json.asRight
      }
    )

  private val tooDeep: DecodeFailure = InvalidMessageBodyFailure("Maximum allowed JSON depth exceeded")

  private def isTooDeep(e: Throwable): Boolean =
    e match {
      case _: MaxDepthExceededException  => true
      case ParsingFailure(_, underlying) => isTooDeep(underlying)
      case _                             => Option(e.getCause).exists(isTooDeep)
    }

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
