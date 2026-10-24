/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.service

import cats.effect._
import cats.implicits._

import io.circe.Json
import io.circe.syntax._

import sttp.model.StatusCode
import sttp.tapir.{Codec, CodecFormat, PublicEndpoint, path, stringBody, stringBodyUtf8AnyFormat}
import sttp.tapir.json.circe.schemaForCirceJson

import com.snowplowanalytics.iglu.server.Utils
import com.snowplowanalytics.iglu.server.http.{Api, Schemas}
import com.snowplowanalytics.iglu.server.http.Endpoints._
import com.snowplowanalytics.iglu.server.http.Schemas.schemaDraftSchema
import com.snowplowanalytics.iglu.server.storage.Storage
import com.snowplowanalytics.iglu.server.model.{DraftVersion, IgluResponse, Permission, SchemaDraft}

object DraftService {

  private val draftPath =
    path[String]("vendor") / path[String]("name") / path[String]("format") /
      path[DraftVersion]("version").description("Draft version")

  def getDraftEndpoint: PublicEndpoint[(String, String, String, DraftVersion), Failure, SchemaDraft, Any] =
    base(StatusCode.NotFound).get.in(draftPath).out(jsonOut[SchemaDraft]).summary("Get a particular draft by its URI")

  def putDraftEndpoint: PublicEndpoint[
    (String, String, String, DraftVersion, Boolean, String),
    Failure,
    (StatusCode, String),
    Any
  ] =
    base(StatusCode.Forbidden)
      .put
      .in(draftPath)
      .in(queryParam[Boolean]("isPublic", Some(false))(booleanCodec).description("Should schema be created as public"))
      .in(jsonIn[Json])
      .out(statuses(StatusCode.NotImplemented).and(stringBody))
      .summary("Add or update a draft")

  /**
    * The drafts are concatenated JSON objects, not a JSON array
    * TODO: list them as a JSON array once the HTTP surface does not need to stay the same as with rho
    */
  def listDraftsEndpoint: PublicEndpoint[Unit, Failure, String, Any] =
    base()
      .get
      .in(root)
      .out(stringBodyUtf8AnyFormat(Codec.string.format(CodecFormat.Json()).schema(Schemas.schemaDrafts)))
      .summary("List all available drafts")

  def api[F[_]: Async](maxJsonDepth: Int)(db: Storage[F]): Api[F] = {
    val schemaBody = Utils.jsonDecoderWithDepthCheck[F](maxJsonDepth)

    Api(
      List(
        Api.withPermission(getDraftEndpoint) { permission =>
          {
            case (vendor, name, format, version) =>
              val draftId  = SchemaDraft.DraftId(vendor, name, format, version)
              val notFound = Failure.json(StatusCode.NotFound, IgluResponse.SchemaNotFound)
              if (permission.canRead(draftId.vendor)) db.getDraft(draftId).map(_.toRight(notFound))
              else Async[F].pure(notFound.asLeft)
          }
        },
        Api.withPermission(putDraftEndpoint) { permission =>
          {
            case (vendor, name, format, version, isPublic, body) =>
              val draftId = SchemaDraft.DraftId(vendor, name, format, version)
              withBody(body, schemaBody) { json =>
                if (permission.canCreateSchema(draftId.vendor))
                  db.addDraft(draftId, json, isPublic).as((StatusCode.NotImplemented, "TODO").asRight[Failure])
                else
                  Async[F].pure(Failure.json(StatusCode.Forbidden, IgluResponse.Forbidden).asLeft)
              }
          }
        },
        Api.withPermission(listDraftsEndpoint) { permission => _ =>
          db.getDrafts
            .filter(isReadable(permission))
            .compile
            .toList
            .map(drafts => drafts.map(_.asJson.noSpaces).mkString.asRight[Failure])
        }
      )
    )
  }

  /** Extract schemas from database, available for particular permission */
  def isReadable(permission: Permission)(schema: SchemaDraft): Boolean =
    permission.canRead(schema.schemaMap.vendor) || schema.metadata.isPublic
}
