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

import cats.Applicative
import cats.data.{Kleisli, NonEmptyList, OptionT}
import cats.implicits._
import cats.effect.Async

import io.circe.syntax._

import org.http4s.{AuthedRoutes, HttpRoutes, MediaType, Method, Response, Status, Uri}
import org.http4s.headers.{Allow, `Content-Type`}

import sttp.apispec.openapi.{Info, Server => OpenApiServer}
import sttp.apispec.openapi.circe_openapi_3_0_3._
import sttp.model.StatusCode
import sttp.tapir.{AnyEndpoint, DecodeResult, EndpointInput, PublicEndpoint, emptyInput}
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.internal.RichEndpointInput // asVectorOfBasicInputs, to list the path inputs of an endpoint
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.{Context, Http4sServerInterpreter, Http4sServerOptions, RichHttp4sEndpoint}
import sttp.tapir.server.interceptor.DecodeFailureContext
import sttp.tapir.server.interceptor.decodefailure.{DecodeFailureHandler, DefaultDecodeFailureHandler}
import sttp.tapir.server.interceptor.decodefailure.DefaultDecodeFailureHandler.FailureMessages
import sttp.tapir.server.interceptor.reject.RejectHandler
import sttp.tapir.server.model.ValuedEndpointOutput

import com.snowplowanalytics.iglu.server.generated.BuildInfo
import com.snowplowanalytics.iglu.server.http.Endpoints.{Failure, IgluDecodeError}
import com.snowplowanalytics.iglu.server.model.Permission

/**
  * The endpoints of a service, interpreted as http4s routes
  *
  * The order of the endpoints matters. When a path segment fails to decode with an `IgluDecodeError`
  * (e.g. an invalid SchemaVer), `decodeFailureHandler` responds right away, while tapir would try the next
  * endpoint. So an endpoint using an Iglu codec in its path shadows the endpoints after it with the same
  * path shape: list the more specific one first (e.g. `/vendor/name/jsonschema/model` in `SchemaService`,
  * whose order s9 and s10 in `HttpSurfaceSpec` pin).
  *
  * @param endpoints tapir endpoints, tried in order, whose server logic gets the caller's permission (see `withPermission`)
  * @param fallbackRoutes routes tried when no endpoint matches, for behaviour tapir cannot express
  */
final case class Api[F[_]: Async](
  endpoints: List[ServerEndpoint[Context[Permission], F]],
  fallbackRoutes: HttpRoutes[F]
) {

  /**
    * The endpoints are interpreted once, as routes with the permission resolved by `PermissionMiddleware` as context
    *
    * @param docsBasePath path where the routes are mounted, used as server URL in the OpenAPI documentation
    */
  def routes(docsBasePath: String): AuthedRoutes[Permission, F] = {
    // Without the permission input, which is no input of the request, but would be documented as a 400
    val definitions = endpoints.map(_.endpoint.copy(securityInput = emptyInput))
    val docs        = Api.docs[F](definitions, docsBasePath)
    val notAllowed  = Api.methodNotAllowed[F](definitions)
    val tapirRoutes = Http4sServerInterpreter[F](Api.options[F]).toContextRoutes(endpoints)

    Kleisli(authed =>
      docs(authed.req) <+> tapirRoutes(authed) <+> notAllowed(authed.req) <+> fallbackRoutes(authed.req)
    )
  }
}

object Api {

  /** The endpoints of a service, without fallback routes */
  def apply[F[_]: Async](endpoints: List[ServerEndpoint[Context[Permission], F]]): Api[F] =
    Api(endpoints, HttpRoutes.empty[F])

  /**
    * The server endpoint of `endpoint`, whose logic gets the permission of the caller: the context of the request,
    * which `PermissionMiddleware` resolves, as security input
    */
  def withPermission[F[_]: Applicative, I, O](endpoint: PublicEndpoint[I, Failure, O, Any])(
    logic: Permission => I => F[Either[Failure, O]]
  ): ServerEndpoint[Context[Permission], F] =
    endpoint.contextSecurityIn[Permission]().serverSecurityLogicSuccess[Permission, F](_.pure[F]).serverLogic(logic)

  /** Preserve previous behavior with rho */
  def decodeFailureHandler(ctx: DecodeFailureContext): Option[ValuedEndpointOutput[_]] =
    (ctx.failure, ctx.failingInput) match {
      case (DecodeResult.Error(_, e: IgluDecodeError), _) =>
        respond(Failure.text(e.status, e.message))
      case (DecodeResult.Missing, q: EndpointInput.Query[_])
          if ctx.request.queryParameters.getMulti(q.name).exists(_.isEmpty) =>
        respond(Failure.text(StatusCode.BadRequest, s"Value of query parameter '${q.name}' missing"))
      case (DecodeResult.Missing, q: EndpointInput.Query[_]) =>
        respond(Failure.text(StatusCode.BadRequest, s"Missing query param: ${q.name}"))
      case (_, _: EndpointInput.PathCapture[_]) =>
        None // A path segment that cannot be decoded doesn't match the endpoint: try the next one
      case _ =>
        DefaultDecodeFailureHandler.respond(ctx).map {
          case (status, headers) =>
            DefaultDecodeFailureHandler.failureResponse(status, headers, FailureMessages.failureMessage(ctx))
        }
    }

  private def respond(failure: Failure): Option[ValuedEndpointOutput[_]] =
    Some(ValuedEndpointOutput(Endpoints.failureOutput(), failure))

  /**
    * Requests that match no endpoint are passed on to the next routes (no 404 or 405 from tapir),
    * errors raised by the server logic to http4s (no 500 from tapir), and the `Accept` header is ignored
    * (no 406 from tapir), as tapir 0.17 did
    */
  private def options[F[_]: Async]: Http4sServerOptions[F] =
    Http4sServerOptions
      .customiseInterceptors[F]
      .decodeFailureHandler(DecodeFailureHandler.pure[F](decodeFailureHandler))
      .rejectHandler(RejectHandler.pure[F](_ => None))
      .notAcceptableInterceptor(None)
      .exceptionHandler(None)
      .serverLog(None)
      .options

  /**
    * As with previous rho, a request whose path matches endpoints, but not its method, gets a 405 listing
    * the methods of these endpoints. tapir would not match the request at all.
    * Like rho, a path segment that fails to decode with an `IgluDecodeError` takes precedence over the 405,
    * whatever the method, e.g. `PATCH /vendor/name/jsonschema/1` is a 400 for the invalid SchemaVer.
    */
  def methodNotAllowed[F[_]: Async](endpoints: List[AnyEndpoint]): HttpRoutes[F] =
    Kleisli { req =>
      val segments = req.pathInfo.renderString.split('/').toList.filter(_.nonEmpty).map(Uri.decode(_))
      val matches  = endpoints.flatMap(e => matchPath(e, segments))
      val methods  = matches.collect { case Right(m) => m }.distinct
      (matches.collectFirst { case Left(e) => e }, NonEmptyList.fromList(methods)) match {
        case (Some(e), _) =>
          OptionT.pure[F](Response[F](Status.fromInt(e.status.code).getOrElse(Status.BadRequest)).withEntity(e.message))
        // As with rho, no 405 for an OPTIONS request, e.g. a CORS preflight that the CORS middleware did not answer
        case (None, Some(ms)) if req.method =!= Method.OPTIONS =>
          val message = s"${req.method} not allowed. Defined methods: ${ms.toList.mkString(", ")}\n"
          OptionT.pure[F](
            Response[F](Status.MethodNotAllowed).withEntity(message).putHeaders(Allow(ms.toList.toSet))
          )
        case _ => OptionT.none
      }
    }

  /**
    * The method of the endpoint if the path matches its path, or the error of a path segment matching a capture
    * of the endpoint that fails to decode with an `IgluDecodeError`. `None` if the path does not match otherwise.
    */
  private def matchPath(endpoint: AnyEndpoint, segments: List[String]): Option[Either[IgluDecodeError, Method]] = {
    val inputs = endpoint.input.asVectorOfBasicInputs().toList
    val path = inputs.collect {
      case EndpointInput.FixedPath(s, _, _) if s.nonEmpty => Right(s)
      case p: EndpointInput.PathCapture[_]                => Left(p)
    }
    val decoded =
      if (path.length === segments.length)
        path.zip(segments).traverse {
          case (Right(fixed), segment) => if (fixed === segment) Some(().asRight) else None
          case (Left(capture), segment) =>
            capture.codec.decode(segment) match {
              case DecodeResult.Value(_)                     => Some(().asRight)
              case DecodeResult.Error(_, e: IgluDecodeError) => Some(e.asLeft)
              case _                                         => None
            }
        }
      else None
    val method = inputs.collectFirst { case EndpointInput.FixedMethod(m, _, _) => m.method }
    decoded.flatMap(_.sequence_.traverse(_ => method.flatMap(m => Method.fromString(m).toOption)))
  }

  /**
    * As rho did, an endpoint is tagged with the first element of its path, `/` for the root,
    * so that Swagger UI groups the endpoints by it
    */
  private def tag(endpoint: AnyEndpoint): String =
    endpoint
      .input
      .asVectorOfBasicInputs()
      .collectFirst {
        case EndpointInput.FixedPath(s, _, _) if s.nonEmpty => s
        case EndpointInput.PathCapture(Some(name), _, _)    => name
      }
      .getOrElse("/")

  /** OpenAPI documentation of the endpoints, in version 3.0.3 as Swagger UI 4 does not support 3.1 */
  def docs[F[_]: Async](endpoints: List[AnyEndpoint], basePath: String): HttpRoutes[F] = {
    val openApi = OpenAPIDocsInterpreter()
      .toOpenAPI(endpoints.map(e => e.tag(tag(e))), Info("Iglu Server API", BuildInfo.version))
      .copy(openapi = "3.0.3", servers = List(OpenApiServer(basePath)))
    val json = openApi.asJson.spaces2
    Kleisli { req =>
      if (req.method === Method.GET && req.pathInfo.renderString === "/swagger.json")
        OptionT.pure[F](
          Response[F](Status.Ok).withEntity(json).withContentType(`Content-Type`(MediaType.application.json))
        )
      else OptionT.none
    }
  }
}
