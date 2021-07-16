/*
 * Copyright (c) 2019-2021 Snowplow Analytics Ltd. All rights reserved.
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
