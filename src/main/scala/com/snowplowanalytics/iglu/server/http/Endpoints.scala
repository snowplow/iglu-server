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
package http

import java.util.UUID

import cats.implicits._
import cats.effect.Concurrent

import eu.timepit.refined.types.numeric.NonNegInt

import io.circe.Encoder
import io.circe.syntax._

import org.http4s.{EntityDecoder, HttpVersion, Request, Status}

import sttp.model.StatusCode
import sttp.tapir.{
  Codec,
  CodecFormat,
  DecodeResult,
  EndpointIO,
  EndpointInput,
  EndpointOutput,
  PublicEndpoint,
  Schema,
  auth,
  endpoint,
  header,
  query,
  statusCode,
  stringBody,
  stringBodyUtf8AnyFormat,
  stringToPath
}
import sttp.tapir.CodecFormat.TextPlain

import com.snowplowanalytics.iglu.core.{ParseError, SchemaVer}

import com.snowplowanalytics.iglu.server.middleware.PermissionMiddleware
import com.snowplowanalytics.iglu.server.model.{DraftVersion, IgluResponse, Schema => IgluSchema}

/** Building blocks shared by the tapir endpoints of all services */
object Endpoints {

  /**
    * Any response that is not the successful output of an endpoint.
    * Errors from the services are JSON, while errors from decoding a request are plain text,
    * as rho and http4s used to return them. `BadRequestHandler` wraps 4xx plain text into JSON.
    */
  final case class Failure(status: StatusCode, contentType: String, body: String)

  object Failure {
    val Json = "application/json"
    val Text = "text/plain; charset=UTF-8"

    def json(status: StatusCode, response: IgluResponse): Failure =
      Failure(status, Json, (response: IgluResponse).asJson.noSpaces)

    def text(status: StatusCode, message: String): Failure =
      Failure(status, Text, message)
  }

  /**
    * A status code output, documenting the given statuses with their reason phrase (e.g. `Not Found`),
    * as rho did. Without documented statuses, the OpenAPI documentation has a `default` response.
    */
  def statuses(codes: StatusCode*): EndpointOutput.StatusCode[StatusCode] =
    codes.foldLeft(statusCode) { (output, code) =>
      output.description(code, Status.fromInt(code.code).fold(_ => code.toString, _.reason))
    }

  /** The output of a `Failure`, documenting the given error statuses */
  def failureOutput(errors: StatusCode*): EndpointOutput[Failure] =
    statuses(errors: _*)
      .and(header[String]("Content-Type"))
      .and(stringBody.schema(Schemas.as[String](Schemas.igluResponseSchema)))
      .mapTo[Failure]

  /** Request parameter that cannot be parsed. The decode failure handler turns it into a `Failure.text` */
  final case class IgluDecodeError(status: StatusCode, message: String) extends Exception(message)

  object IgluDecodeError {
    def badRequest(message: String): IgluDecodeError = IgluDecodeError(StatusCode.BadRequest, message)
  }

  private def parsing[T](parse: String => Either[IgluDecodeError, T])(print: T => String): Codec[String, T, TextPlain] =
    Codec.string.mapDecode(s => parse(s).fold(e => DecodeResult.Error(s, e), DecodeResult.Value(_)))(print)

  val EmptyVersion = "Version cannot be empty, should be model to get SchemaList or full SchemaVer"

  def parseSchemaVer(s: String): Either[IgluDecodeError, SchemaVer.Full] =
    SchemaVer.parseFull(s) match {
      case Right(v) => v.asRight
      case Left(ParseError.InvalidSchemaVer) if s.isEmpty =>
        IgluDecodeError(StatusCode.NotFound, EmptyVersion).asLeft
      case Left(e) =>
        IgluDecodeError.badRequest(s"Cannot parse version part '$s' as SchemaVer, ${e.code}").asLeft
    }

  implicit val schemaVerCodec: Codec[String, SchemaVer.Full, TextPlain] =
    parsing(parseSchemaVer)(_.asString)

  implicit val reprFormatCodec: Codec[String, IgluSchema.Repr.Format, TextPlain] =
    parsing { s =>
      IgluSchema.Repr.Format.parse(s).toRight(IgluDecodeError.badRequest(s"Unknown schema representation format: '$s'"))
    }(_.toString.toLowerCase)

  implicit val schemaFormatCodec: Codec[String, IgluSchema.Format, TextPlain] =
    parsing(s => IgluSchema.Format.parse(s).toRight(IgluDecodeError.badRequest(s"Unknown schema format: '$s'")))(
      _.toString.toLowerCase
    )

  implicit val draftVersionCodec: Codec[String, DraftVersion, TextPlain] =
    parsing { s =>
      Either
        .catchOnly[NumberFormatException](s.toInt)
        .leftMap(_ => s"$s is not an integer")
        .flatMap(NonNegInt.from)
        .leftMap(IgluDecodeError.badRequest)
    }(_.value.toString)

  val booleanCodec: Codec[String, Boolean, TextPlain] =
    parsing {
      case "true"  => true.asRight
      case "false" => false.asRight
      case s       => IgluDecodeError.badRequest(s"Invalid boolean format: '$s'").asLeft
    }(_.toString).schema(Schema.schemaForBoolean)

  val uuidCodec: Codec[String, UUID, TextPlain] =
    parsing { s =>
      Either.catchNonFatal(UUID.fromString(s)).leftMap(_ => IgluDecodeError.badRequest(s"Invalid uuid format: $s"))
    }(_.toString).schema(Schema.schemaForUUID)

  /**
    * A query parameter, using its first value as rho did.
    * Without value, it is the default if there is one, or a `Missing` decode failure otherwise.
    */
  def queryParam[T](name: String, default: Option[T])(
    implicit base: Codec[String, T, TextPlain]
  ): EndpointInput.Query[T] = {
    val codec = Codec
      .list(Codec.string)
      .mapDecode {
        case first :: _ => base.decode(first)
        case Nil        => default.fold[DecodeResult[T]](DecodeResult.Missing)(DecodeResult.Value(_))
      }(t => List(base.encode(t)))
      .schema(base.schema)
    val input = query[T](name)(codec)
    default.fold(input)(input.default)
  }

  /**
    * The `apikey` header, only for the documentation: `PermissionMiddleware` authenticates the request,
    * and the server logic of the endpoints gets the resulting `Permission` (see `Api`).
    * It accepts repeated headers, which `header[Option[String]]` would reject with a 400.
    */
  val apiKey: EndpointInput[Unit] = {
    val codec = Codec.list(Codec.string).map(_.headOption)(_.toList).schema(Schema.schemaForString.asOption)
    auth.apiKey(header[Option[String]](PermissionMiddleware.ApiKey)(codec)).map(_ => ())(_ => None)
  }

  /**
    * Base for all endpoints: the `apikey` header, and any `Failure` as error output
    *
    * @param errors the error statuses of the endpoint, for the documentation
    */
  def base(errors: StatusCode*): PublicEndpoint[Unit, Failure, Unit, Any] =
    endpoint.in(apiKey).errorOut(failureOutput(errors: _*))

  /** The empty path, matching `/` and nothing (an endpoint without path input matches any path) */
  val root: EndpointInput[Unit] = ""

  /** JSON response, for types that only have a circe `Encoder` */
  def jsonOut[T: Encoder: Schema]: EndpointIO.Body[String, T] =
    stringBodyUtf8AnyFormat(
      Codec
        .json[T](s => DecodeResult.Error(s, new UnsupportedOperationException("Response body only")))(_.asJson.noSpaces)
    )

  /** JSON request body, decoded by the server logic with an http4s `EntityDecoder` (see `withBody`) */
  def jsonIn[T: Schema]: EndpointIO.Body[String, String] =
    stringBodyUtf8AnyFormat(Codec.string.format(CodecFormat.Json()).schema(Schemas.as[String](implicitly[Schema[T]])))

  /** Decode a request body with `decodeBody`, then run the server logic on it, or return the decoding failure */
  def withBody[F[_]: Concurrent, A, B](body: String, decoder: EntityDecoder[F, A])(
    f: A => F[Either[Failure, B]]
  ): F[Either[Failure, B]] =
    decodeBody(body, decoder).flatMap(_.fold(failure => Concurrent[F].pure(failure.asLeft), f))

  /**
    * Decode a request body as rho did, with the services' http4s decoders. These check the JSON depth,
    * and their failures are plain text: 400 for malformed JSON, 422 for invalid JSON.
    */
  private def decodeBody[F[_]: Concurrent, A](body: String, decoder: EntityDecoder[F, A]): F[Either[Failure, A]] =
    decoder.decode(Request[F]().withEntity(body), strict = false).value.flatMap {
      case Right(a) => Concurrent[F].pure(a.asRight)
      case Left(failure) =>
        val response = failure.toHttpResponse[F](HttpVersion.`HTTP/1.1`)
        response.bodyText.compile.foldMonoid.map(text => Failure.text(StatusCode(response.status.code), text).asLeft)
    }
}
