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
