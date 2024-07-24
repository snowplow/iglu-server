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

import java.util.UUID

import cats.effect._
import cats.implicits._

import io.circe._

import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.rho.{AuthedContext, RhoMiddleware, RhoRoutes}
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.rho.swagger.syntax.io

import com.snowplowanalytics.iglu.server.storage.Storage
import com.snowplowanalytics.iglu.server.middleware.PermissionMiddleware
import com.snowplowanalytics.iglu.server.model.{DraftVersion, IgluResponse, Permission, SchemaDraft}
import com.snowplowanalytics.iglu.server.codecs.UriParsers._
import com.snowplowanalytics.iglu.server.codecs.JsonCodecs._

class DraftService[F[+_]: Sync](swagger: SwaggerSyntax[F], db: Storage[F], ctx: AuthedContext[F, Permission])
    extends RhoRoutes[F] {
  import swagger._
  import DraftService._
  implicit val C: Clock[F] = Clock.create[F]

  val version    = pathVar[DraftVersion]("version", "Draft version")
  val isPublic   = paramD[Boolean]("isPublic", false, "Should schema be created as public")
  val schemaBody = jsonOf[F, Json]

  "Get a particular draft by its URI" **
    GET / 'vendor / 'name / 'format / version >>> ctx.auth |>> getDraft _

  "Add or update a draft" **
    PUT / 'vendor / 'name / 'format / version +? isPublic >>> ctx.auth ^ schemaBody |>> putDraft _

  "List all available drafts" **
    GET >>> ctx.auth |>> listDrafts _

  def getDraft(vendor: String, name: String, format: String, version: DraftVersion, permission: Permission) = {
    val draftId = SchemaDraft.DraftId(vendor, name, format, version)
    if (permission.canRead(draftId.vendor)) {
      db.getDraft(draftId).flatMap {
        case Some(draft) => Ok(draft)
        case _           => NotFound(IgluResponse.SchemaNotFound: IgluResponse)
      }
    } else NotFound(IgluResponse.SchemaNotFound: IgluResponse)
  }

  def putDraft(
    vendor: String,
    name: String,
    format: String,
    version: DraftVersion,
    isPublic: Boolean,
    permission: Permission,
    body: Json
  ) = {
    val draftId = SchemaDraft.DraftId(vendor, name, format, version)
    if (permission.canCreateSchema(draftId.vendor))
      db.addDraft(draftId, body, isPublic) *> NotImplemented("TODO")
    else
      Forbidden(IgluResponse.Forbidden: IgluResponse)
  }

  def listDrafts(permission: Permission) =
    Ok(db.getDrafts.filter(isReadable(permission)))
}

object DraftService {

  def asRoutes(
    db: Storage[IO],
    superKey: Option[UUID],
    ctx: AuthedContext[IO, Permission],
    rhoMiddleware: RhoMiddleware[IO]
  ): HttpRoutes[IO] = {
    val service = new DraftService(io, db, ctx).toRoutes(rhoMiddleware)
    PermissionMiddleware.wrapService(db, superKey, ctx, service)
  }

  /** Extract schemas from database, available for particular permission */
  def isReadable(permission: Permission)(schema: SchemaDraft): Boolean =
    permission.canRead(schema.schemaMap.vendor) || schema.metadata.isPublic
}
