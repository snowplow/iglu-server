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
package service

import cats.data.{Kleisli, OptionT, Validated}
import cats.effect._
import cats.implicits._

import fs2.Stream

import io.circe.Json

import org.http4s.{HttpRoutes, Method, Response, Status, Uri}

import sttp.model.StatusCode
import sttp.tapir.{PublicEndpoint, path, stringToPath}
import sttp.tapir.json.circe.schemaForCirceJson

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.implicits._

import com.snowplowanalytics.iglu.schemaddl.jsonschema.Linter
import com.snowplowanalytics.iglu.server.http.Api
import com.snowplowanalytics.iglu.server.http.Endpoints._
import com.snowplowanalytics.iglu.server.http.Schemas._
import com.snowplowanalytics.iglu.server.storage.Storage
import com.snowplowanalytics.iglu.server.model.{IgluResponse, Permission, Schema, VersionCursor}
import com.snowplowanalytics.iglu.server.model.Schema.{SchemaBody, SupersedingInfo}
import com.snowplowanalytics.iglu.server.model.Schema.Repr.{Format => SchemaFormat}
import com.snowplowanalytics.iglu.server.model.VersionCursor.Inconsistency

object SchemaService {

  private def repr(default: SchemaFormat) =
    queryParam[SchemaFormat]("repr", Some(default)).description("Schema representation format")

  private val isPublic =
    queryParam[Boolean]("isPublic", Some(false))(booleanCodec).description("Should schema be created as public")

  private val vendor = path[String]("vendor")
  private val name   = path[String]("name")
  private val format = path[String]("format")

  private val schemaPath = vendor / name / format / path[SchemaVer.Full]("version").description("SchemaVer")

  private val uploaded = statuses(StatusCode.Ok, StatusCode.Created).and(jsonOut[IgluResponse])

  private val uploadErrors = List(StatusCode.BadRequest, StatusCode.Forbidden, StatusCode.Conflict)

  def getSchemaEndpoint
    : PublicEndpoint[(String, String, String, SchemaVer.Full, SchemaFormat), Failure, Schema.Repr, Any] =
    base(StatusCode.NotFound)
      .get
      .in(schemaPath)
      .in(repr(SchemaFormat.Canonical))
      .out(jsonOut[Schema.Repr])
      .summary("Get a particular schema by its Iglu URI")

  def deleteSchemaEndpoint: PublicEndpoint[(String, String, String, SchemaVer.Full), Failure, IgluResponse, Any] =
    base(StatusCode.Unauthorized, StatusCode.MethodNotAllowed)
      .delete
      .in(schemaPath)
      .out(jsonOut[IgluResponse])
      .summary("Delete a particular schema from Iglu by its URI")

  def getSchemasByNameEndpoint: PublicEndpoint[(String, String, SchemaFormat), Failure, List[Schema.Repr], Any] =
    base(StatusCode.NotFound)
      .get
      .in(vendor / name)
      .in(repr(SchemaFormat.Uri))
      .out(jsonOut[List[Schema.Repr]])
      .summary("Get list of schemas by vendor name and schema name")

  def getSchemasByModelEndpoint: PublicEndpoint[(String, String, Int, SchemaFormat), Failure, List[Schema.Repr], Any] =
    base(StatusCode.NotFound)
      .get
      .in(vendor / name / "jsonschema" / path[Int]("model"))
      .in(repr(SchemaFormat.Uri))
      .out(jsonOut[List[Schema.Repr]])
      .summary("Get list of schemas by vendor name, schema name and major version (model)")

  def getSchemasByVendorEndpoint: PublicEndpoint[(String, SchemaFormat), Failure, List[Schema.Repr], Any] =
    base(StatusCode.NotFound)
      .get
      .in(vendor)
      .in(repr(SchemaFormat.Uri))
      .out(jsonOut[List[Schema.Repr]])
      .summary("Get list of schemas by vendor name")

  def listSchemasEndpoint: PublicEndpoint[SchemaFormat, Failure, List[Schema.Repr], Any] =
    base().get.in(root).in(repr(SchemaFormat.Uri)).out(jsonOut[List[Schema.Repr]]).summary("Get list of schemas (all)")

  def putSchemaEndpoint: PublicEndpoint[
    (String, String, String, SchemaVer.Full, Boolean, String),
    Failure,
    (StatusCode, IgluResponse),
    Any
  ] =
    base(uploadErrors: _*)
      .put
      .in(schemaPath)
      .in(isPublic)
      .in(jsonIn[SchemaBody])
      .out(uploaded)
      .summary("Add a schema (self-describing or not) to its Iglu URI")

  def postSchemaEndpoint: PublicEndpoint[(Boolean, String), Failure, (StatusCode, IgluResponse), Any] =
    base(uploadErrors: _*)
      .post
      .in(root)
      .in(isPublic)
      .in(jsonIn[SchemaBody])
      .out(uploaded)
      .summary("Publish new self-describing schema")

  def validateEndpoint: PublicEndpoint[(String, String, String, String), Failure, IgluResponse, Any] =
    base(StatusCode.Forbidden)
      .post
      .in("validate" / vendor / name / "jsonschema" / path[String]("version"))
      .in(jsonIn[Json])
      .out(jsonOut[IgluResponse])
      .summary("Schema validation endpoint (deprecated)")

  /**
    * Paths that match no endpoint, but that rho rejected, whatever the method, while matching them
    * against `/vendor/name/format/version` and `/vendor/name/jsonschema/model`: an invalid or empty version,
    * or a model that is not an integer. rho kept a trailing slash as an empty last segment. An OPTIONS request,
    * which `Api.methodNotAllowed` leaves through, is rejected likewise on `/vendor/name/jsonschema/version`.
    */
  private def rhoPathErrors[F[_]: Sync]: HttpRoutes[F] =
    Kleisli { req =>
      val failure = rhoSegments(req.pathInfo.renderString) match {
        case _ :: _ :: format :: version :: rest if rest.nonEmpty || version.isEmpty || req.method === Method.OPTIONS =>
          parseSchemaVer(version) match {
            case Left(e) =>
              Some((Status.fromInt(e.status.code).getOrElse(Status.BadRequest), e.message))
            case Right(_) if format === "jsonschema" =>
              Some((Status.BadRequest, s"Invalid number format: '$version'"))
            case Right(_) =>
              None
          }
        case _ => None
      }
      OptionT.fromOption[F](failure.map { case (status, message) => Response[F](status).withEntity(message) })
    }

  /** Path segments as rho split them: empty segments are skipped, except the last one */
  private def rhoSegments(path: String): List[String] = {
    val segments = path.stripPrefix("/").split("/", -1).toList.map(Uri.decode(_))
    segments.init.filter(_.nonEmpty) :+ segments.last
  }

  def api[F[_]: Async](patchesAllowed: Boolean, webhooks: Webhook.WebhookClient[F], maxJsonDepth: Int)(
    db: Storage[F]
  ): Api[F] = {
    val handlers = new Handlers[F](db, patchesAllowed, webhooks, maxJsonDepth)
    import handlers._

    Api(
      List(
        // Before getSchemaEndpoint: /vendor/name/jsonschema/1 is a model, not an invalid SchemaVer
        Api.withPermission(getSchemasByModelEndpoint)(p => (getSchemasByModel(p) _).tupled),
        Api.withPermission(getSchemaEndpoint)(p => (getSchema(p) _).tupled),
        Api.withPermission(deleteSchemaEndpoint)(p => (deleteSchema(p) _).tupled),
        Api.withPermission(putSchemaEndpoint)(p => (putSchema(p) _).tupled),
        Api.withPermission(getSchemasByNameEndpoint)(p => (getSchemasByName(p) _).tupled),
        Api.withPermission(getSchemasByVendorEndpoint)(p => (getSchemasByVendor(p) _).tupled),
        Api.withPermission(listSchemasEndpoint)(listSchemas),
        Api.withPermission(postSchemaEndpoint)(p => (postSchema(p) _).tupled),
        // Only the body is validated, the path is ignored
        Api.withPermission(validateEndpoint)(p => input => validate(p)(input._4))
      ),
      rhoPathErrors
    )
  }

  private class Handlers[F[_]: Concurrent](
    db: Storage[F],
    patchesAllowed: Boolean,
    webhooks: Webhook.WebhookClient[F],
    maxJsonDepth: Int
  ) {

    private val schemaOrJson = Utils.jsonOfWithDepthCheck[F, SchemaBody](maxJsonDepth)
    private val jsonBody     = Utils.jsonDecoderWithDepthCheck[F](maxJsonDepth)

    private def notFound(response: IgluResponse) = Failure.json(StatusCode.NotFound, response)

    def getSchema(permission: Permission)(
      vendor: String,
      name: String,
      format: String,
      version: SchemaVer.Full,
      schemaFormat: SchemaFormat
    ): F[Either[Failure, Schema.Repr]] =
      db.getSchema(SchemaMap(vendor, name, format, version)).map {
        case Some(schema) if schema.metadata.isPublic => schema.withFormat(schemaFormat).asRight
        case Some(schema) if permission.canRead(schema.schemaMap.schemaKey.vendor) =>
          schema.withFormat(schemaFormat).asRight
        case _ => notFound(IgluResponse.SchemaNotFound).asLeft
      }

    def deleteSchema(permission: Permission)(
      vendor: String,
      name: String,
      format: String,
      version: SchemaVer.Full
    ): F[Either[Failure, IgluResponse]] =
      permission match {
        case Permission.Super if patchesAllowed =>
          db.deleteSchema(SchemaMap(vendor, name, format, version))
            .as((IgluResponse.Message("Schema deleted"): IgluResponse).asRight)
        case Permission.Super =>
          Concurrent[F].pure(
            Failure
              .json(StatusCode.MethodNotAllowed, IgluResponse.Message("DELETE is forbidden on production registry"))
              .asLeft
          )
        case _ =>
          Concurrent[F].pure(
            Failure.json(StatusCode.Unauthorized, IgluResponse.Message("Not enough permissions")).asLeft
          )
      }

    def getSchemasByName(permission: Permission)(
      vendor: String,
      name: String,
      format: SchemaFormat
    ): F[Either[Failure, List[Schema.Repr]]] =
      schemasOrNotFound(db.getSchemasByName(vendor, name).filter(isReadable(permission)).map(_.withFormat(format)))

    def getSchemasByVendor(permission: Permission)(
      vendor: String,
      format: SchemaFormat
    ): F[Either[Failure, List[Schema.Repr]]] =
      schemasOrNotFound(db.getSchemasByVendor(vendor).filter(isReadable(permission)).map(_.withFormat(format)))

    def getSchemasByModel(permission: Permission)(
      vendor: String,
      name: String,
      model: Int,
      format: SchemaFormat
    ): F[Either[Failure, List[Schema.Repr]]] =
      schemasOrNotFound(
        db.getSchemasByModel(vendor, name, model).filter(isReadable(permission)).map(_.withFormat(format))
      )

    def postSchema(permission: Permission)(
      isPublic: Boolean,
      body: String
    ): F[Either[Failure, (StatusCode, IgluResponse)]] =
      withBody(body, schemaOrJson) {
        case _: SchemaBody.BodyOnly =>
          Concurrent[F].pure(Failure.json(StatusCode.BadRequest, IgluResponse.InvalidSchema).asLeft)
        case SchemaBody.SelfDescribing(schema, supersedingInfo) =>
          publishSchema(isPublic, permission, schema, supersedingInfo)
      }

    def putSchema(permission: Permission)(
      vendor: String,
      name: String,
      format: String,
      version: SchemaVer.Full,
      isPublic: Boolean,
      body: String
    ): F[Either[Failure, (StatusCode, IgluResponse)]] =
      withBody(body, schemaOrJson) {
        case SchemaBody.BodyOnly(schema, supersedingInfo) =>
          val schemaMap = SchemaMap(vendor, name, format, version)
          publishSchema(isPublic, permission, SelfDescribingSchema(schemaMap, schema), supersedingInfo)
        case SchemaBody.SelfDescribing(schema, supersedingInfo) =>
          val schemaMapUri = SchemaMap(vendor, name, format, version)
          if (schemaMapUri == schema.self) publishSchema(isPublic, permission, schema, supersedingInfo)
          else
            Concurrent[F].pure(
              Failure
                .json(StatusCode.BadRequest, IgluResponse.SchemaMismatch(schemaMapUri.schemaKey, schema.self.schemaKey))
                .asLeft
            )
      }

    def validate(permission: Permission)(body: String): F[Either[Failure, IgluResponse]] =
      withBody(body, jsonBody) { json =>
        Concurrent[F].pure(ValidationService.validateSchema(Schema.Format.Jsonschema, permission, json, maxJsonDepth))
      }

    def listSchemas(permission: Permission)(format: SchemaFormat): F[Either[Failure, List[Schema.Repr]]] = {
      val result = format match {
        case SchemaFormat.Uri =>
          db.getSchemasKeyOnly
            .map(_.filter(isReadablePair(permission)).map {
              case (map, meta) => Schema(map, meta, Json.Null, SupersedingInfo.empty).withFormat(SchemaFormat.Uri)
            })
        case _ =>
          db.getSchemas.map(_.filter(isReadable(permission)).map(_.withFormat(format)))
      }
      result.map(_.asRight)
    }

    private def publishSchema(
      isPublic: Boolean,
      permission: Permission,
      schema: SelfDescribingSchema[Json],
      supersedingInfo: SupersedingInfo
    ): F[Either[Failure, (StatusCode, IgluResponse)]] =
      if (permission.canCreateSchema(schema.self.schemaKey.vendor)) {
        ValidationService.validateJsonSchema(schema.normalize, maxJsonDepth) match {
          case Validated.Invalid(report) if report.exists(_.level == Linter.Level.Error) =>
            Concurrent[F].pure(Failure.json(StatusCode.BadRequest, IgluResponse.SchemaValidationReport(report)).asLeft)
          case _ => addSchema(schema, isPublic, supersedingInfo, permission)
        }
      } else Concurrent[F].pure(Failure.json(StatusCode.Forbidden, IgluResponse.Forbidden).asLeft)

    /** Make sure that SchemaList can be only non-empty list */
    private def schemasOrNotFound(queryResult: Stream[F, Schema.Repr]): F[Either[Failure, List[Schema.Repr]]] =
      queryResult.compile.toList.map {
        case Nil     => notFound(IgluResponse.Message(s"No schemas available")).asLeft
        case schemas => schemas.asRight
      }

    private def conflict(response: IgluResponse) = Failure.json(StatusCode.Conflict, response)

    private def addSchema(
      schema: SelfDescribingSchema[Json],
      isPublic: Boolean,
      supersedingInfo: SupersedingInfo,
      permission: Permission
    ): F[Either[Failure, (StatusCode, IgluResponse)]] =
      for {
        allowed <- isSchemaAllowed(db, schema.self, patchesAllowed, isPublic, supersedingInfo, permission)
        response <- allowed match {
          case Right(_) =>
            for {
              // Atomic insert-or-update: the DB decides the outcome, closing the race window
              // that otherwise let concurrent uploads bypass patchesAllowed or deadlock.
              wasInserted <- db.upsertSchema(
                schema.self,
                schema.schema,
                isPublic,
                supersedingInfo.supersedes,
                patchesAllowed
              )
              existing = !wasInserted
              // Under normal operation `isSchemaAllowed` (via VersionCursor.isAllowed) already rejects an
              // existing version when patching is disabled, so this branch is unreachable then. It only
              // fires when a concurrent request inserted the row between that check and this upsert - hence
              // AlreadyExists being handled here as well as in the `Left` case below.
              response <- if (existing && !patchesAllowed)
                Concurrent[F].pure(
                  conflict(IgluResponse.Message((Inconsistency.AlreadyExists: Inconsistency).show))
                    .asLeft[(StatusCode, IgluResponse)]
                )
              else {
                val payload: IgluResponse = IgluResponse.SchemaUploaded(existing, schema.self.schemaKey)
                val status                = if (existing) StatusCode.Ok else StatusCode.Created
                webhooks.schemaPublished(schema.self.schemaKey, existing).as((status, payload).asRight[Failure])
              }
            } yield response
          case Left(Inconsistency.NextRevisionExists) =>
            Concurrent[F].pure(conflict(IgluResponse.SchemaNonSequential).asLeft[(StatusCode, IgluResponse)])
          case Left(error) =>
            Concurrent[F].pure(conflict(IgluResponse.Message(error.show)).asLeft[(StatusCode, IgluResponse)])
        }
      } yield response
  }

  def isSchemaAllowed[F[_]: Concurrent](
    db: Storage[F],
    schemaMap: SchemaMap,
    patchesAllowed: Boolean,
    isPublic: Boolean,
    supersedingInfo: SupersedingInfo,
    permission: Permission
  ): F[Either[Inconsistency, Unit]] =
    for {
      allSchemaKeys <- db.getSchemasKeyOnly.map(_.filter(isReadablePair(permission))).map(_.map(_._1.schemaKey))
      schemas       <- db.getSchemasByName(schemaMap.schemaKey.vendor, schemaMap.schemaKey.name).compile.toList
      previousPublic = schemas.forall(_.metadata.isPublic)
      versions       = schemas.map(_.schemaMap.schemaKey.version)
    } yield {
      lazy val isAllowedPatch    = allSchemaKeys.contains(schemaMap.schemaKey) && patchesAllowed
      lazy val vendorNameClashes = filterClashingVendorNames(schemaMap.schemaKey, allSchemaKeys)
      if (schemaMap.schemaKey.version.asString === "1-0-0" && !isAllowedPatch && vendorNameClashes.nonEmpty)
        Inconsistency.VendorNameClash(schemaMap.schemaKey, vendorNameClashes).asLeft
      else if (schemas.nonEmpty && (isPublic != previousPublic))
        Inconsistency.Availability(isPublic, previousPublic).asLeft
      else
        VersionCursor.isAllowed(schemaMap.schemaKey.version, versions, patchesAllowed, supersedingInfo.supersedes)
    }

  /**
    * Filter schema keys whose vendorAsSnakeCase_nameAsSnakeCase clashes with given schema key's vendorAsSnakeCase_nameAsSnakeCase
    *
    * Return set of (vendor, name) tuples
    */
  def filterClashingVendorNames(schemaKey: SchemaKey, allSchemaKeys: List[SchemaKey]): Set[(String, String)] = {
    lazy val target = s"${schemaKey.vendorAsSnakeCase}_${schemaKey.nameAsSnakeCase}"

    allSchemaKeys
      .filter(sk => s"${sk.vendorAsSnakeCase}_${sk.nameAsSnakeCase}" === target)
      .map(sk => (sk.vendor, sk.name))
      .toSet
  }

  /** Extract schemas from database, available for particular permission */
  def isReadable(permission: Permission)(schema: Schema): Boolean =
    isReadablePair(permission)((schema.schemaMap, schema.metadata))

  def isReadablePair(permission: Permission)(meta: (SchemaMap, Schema.Metadata)): Boolean =
    meta match {
      case (schemaMap, metadata) =>
        permission.canRead(schemaMap.schemaKey.vendor) || metadata.isPublic
    }
}
