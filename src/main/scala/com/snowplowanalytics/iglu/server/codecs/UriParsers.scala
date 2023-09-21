/*
 * Copyright (c) 2019-2023 Snowplow Analytics Ltd. All rights reserved.
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
package codecs

import scala.reflect.runtime.universe.TypeTag

import cats.Monad

import eu.timepit.refined.types.numeric.NonNegInt

import org.http4s.rho.bits._
import com.snowplowanalytics.iglu.core.{ParseError, SchemaVer}
import com.snowplowanalytics.iglu.server.model.{DraftVersion, Schema}

trait UriParsers {

  implicit def schemaReprStringParser[F[_]]: StringParser[F, Schema.Repr.Format] =
    new StringParser[F, Schema.Repr.Format] {
      override val typeTag: Some[TypeTag[Schema.Repr.Format]] = Some(implicitly[TypeTag[Schema.Repr.Format]])

      override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Schema.Repr.Format] =
        Schema.Repr.Format.parse(s) match {
          case Some(format) => SuccessResponse(format)
          case None         => FailureResponse.pure[F](BadRequest.pure(s"Unknown schema representation format: '$s'"))
        }
    }

  implicit def schemaFormatStringParser[F[_]]: StringParser[F, Schema.Format] =
    new StringParser[F, Schema.Format] {
      override val typeTag: Some[TypeTag[Schema.Format]] = Some(implicitly[TypeTag[Schema.Format]])

      override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Schema.Format] =
        Schema.Format.parse(s) match {
          case Some(format) => SuccessResponse(format)
          case None         => FailureResponse.pure[F](BadRequest.pure(s"Unknown schema format: '$s'"))
        }
    }

  implicit def schemaVerParser[F[_]]: StringParser[F, SchemaVer.Full] =
    new StringParser[F, SchemaVer.Full] {
      override val typeTag: Some[TypeTag[SchemaVer.Full]] = Some(implicitly[TypeTag[SchemaVer.Full]])

      override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, SchemaVer.Full] =
        SchemaVer.parseFull(s) match {
          case Right(v) => SuccessResponse(v)
          case Left(ParseError.InvalidSchemaVer) if s.isEmpty =>
            FailureResponse.pure[F](
              NotFound.pure(s"Version cannot be empty, should be model to get SchemaList or full SchemaVer")
            )
          case Left(e) =>
            FailureResponse.pure[F](BadRequest.pure(s"Cannot parse version part '$s' as SchemaVer, ${e.code}"))
        }
    }

  implicit def draftVersionParser[F[_]]: StringParser[F, DraftVersion] =
    new StringParser[F, DraftVersion] {
      override val typeTag: Some[TypeTag[DraftVersion]] = Some(implicitly[TypeTag[DraftVersion]])

      override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, DraftVersion] = {
        val int =
          try {
            Right(s.toInt)
          } catch { case _: NumberFormatException => Left(s"$s is not an integer") }
        int.flatMap(NonNegInt.from).fold(err => FailureResponse.pure[F](BadRequest.pure(err)), SuccessResponse.apply)
      }
    }
}

object UriParsers extends UriParsers
