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

import java.util.UUID

import cats.implicits._
import cats.effect.{IO, Sync}

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

import org.http4s._
import org.http4s.circe._
import org.http4s.rho.{RhoMiddleware, RhoRoutes}
import org.http4s.rho.swagger.SwaggerSyntax
import org.http4s.rho.AuthedContext
import org.http4s.rho.swagger.syntax.{io => swaggerSyntax}

import com.snowplowanalytics.iglu.server.middleware.PermissionMiddleware
import com.snowplowanalytics.iglu.server.codecs.JsonCodecs._
import com.snowplowanalytics.iglu.server.model.Permission
import com.snowplowanalytics.iglu.server.model.IgluResponse
import com.snowplowanalytics.iglu.server.storage.Storage

class AuthService[F[+_]: Sync](swagger: SwaggerSyntax[F], ctx: AuthedContext[F, Permission], db: Storage[F])
    extends RhoRoutes[F] {
  import swagger._
  import AuthService._

  val apikey = paramD[UUID]("key", "UUID apikey to delete")

  "Route to delete api key" **
    DELETE / "keygen" +? apikey >>> ctx.auth |>> deleteKey _

  "Route to generate new keys" **
    POST / "keygen" >>> ctx.auth ^ jsonOf[F, GenerateKey] |>> { (authInfo: Permission, gk: GenerateKey) =>
    if (authInfo.key.contains(Permission.KeyAction.Create)) {
      val vendorPrefix = Permission.Vendor.parse(gk.vendorPrefix)
      if (authInfo.canCreatePermission(vendorPrefix.asString)) {
        for {
          keyPair    <- Permission.KeyPair.generate[F]
          _          <- db.addKeyPair(keyPair, vendorPrefix)
          okResponse <- Ok(keyPair.asJson)
        } yield okResponse
      } else {
        Forbidden(IgluResponse.Message(s"Cannot create ${vendorPrefix.show} using your permissions"): IgluResponse)
      }
    } else Forbidden(IgluResponse.Message("Not sufficient privileges to create keys"): IgluResponse)
  }

  def deleteKey(key: UUID, permission: Permission) =
    if (permission.key.contains(Permission.KeyAction.Delete)) {
      db.deletePermission(key) *> Ok(IgluResponse.Message(s"Keys have been deleted"): IgluResponse)
    } else Forbidden("Not sufficient privileges to delete key")
}

object AuthService {

  case class GenerateKey(vendorPrefix: String)

  implicit val schemaGenerateReq: Decoder[GenerateKey] = deriveDecoder[GenerateKey]

  def asRoutes(
    db: Storage[IO],
    superKey: Option[UUID],
    ctx: AuthedContext[IO, Permission],
    rhoMiddleware: RhoMiddleware[IO]
  ): HttpRoutes[IO] = {
    val service = new AuthService(swaggerSyntax, ctx, db).toRoutes(rhoMiddleware)
    PermissionMiddleware.wrapService(db, superKey, ctx, service)
  }
}
