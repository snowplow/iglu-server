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
import cats.effect.{Async, Sync}

import io.circe._
import io.circe.generic.semiauto._

import sttp.model.StatusCode
import sttp.tapir.{PublicEndpoint, stringToPath}

import com.snowplowanalytics.iglu.server.http.Api
import com.snowplowanalytics.iglu.server.http.Endpoints._
import com.snowplowanalytics.iglu.server.http.Schemas.{generateKeySchema, igluResponseSchema, keyPairSchema}
import com.snowplowanalytics.iglu.server.model.Permission
import com.snowplowanalytics.iglu.server.model.IgluResponse
import com.snowplowanalytics.iglu.server.storage.Storage

object AuthService {

  case class GenerateKey(vendorPrefix: String)

  implicit val schemaGenerateReq: Decoder[GenerateKey] = deriveDecoder[GenerateKey]

  def deleteKeyEndpoint: PublicEndpoint[UUID, Failure, IgluResponse, Any] =
    base(StatusCode.Forbidden)
      .delete
      .in("keygen")
      .in(queryParam[UUID]("key", None)(uuidCodec).description("UUID apikey to delete"))
      .out(jsonOut[IgluResponse])
      .summary("Route to delete api key")

  def generateKeyEndpoint: PublicEndpoint[String, Failure, Permission.KeyPair, Any] =
    base(StatusCode.Forbidden)
      .post
      .in("keygen")
      .in(jsonIn[GenerateKey])
      .out(jsonOut[Permission.KeyPair])
      .summary("Route to generate new keys")

  def api[F[_]: Async](maxJsonDepth: Int)(db: Storage[F]): Api[F] = {
    val jsonGenerateKey = Utils.jsonOfWithDepthCheck[F, GenerateKey](maxJsonDepth)

    Api(
      List(
        Api.withPermission(deleteKeyEndpoint) { permission => key =>
          if (permission.key.contains(Permission.KeyAction.Delete))
            db.deletePermission(key)
              .as((IgluResponse.Message(s"Keys have been deleted"): IgluResponse).asRight[Failure])
          else
            Async[F].pure(Failure.text(StatusCode.Forbidden, "Not sufficient privileges to delete key").asLeft)
        },
        Api.withPermission(generateKeyEndpoint) { permission => body =>
          withBody(body, jsonGenerateKey)(generateKey(db, permission, _))
        }
      )
    )
  }

  private def generateKey[F[_]: Sync](
    db: Storage[F],
    authInfo: Permission,
    gk: GenerateKey
  ): F[Either[Failure, Permission.KeyPair]] =
    if (authInfo.key.contains(Permission.KeyAction.Create)) {
      val vendorPrefix = Permission.Vendor.parse(gk.vendorPrefix, true)
      if (authInfo.canCreatePermission(vendorPrefix.asString))
        for {
          keyPair <- Permission.KeyPair.generate[F]
          _       <- db.addKeyPair(keyPair, vendorPrefix)
        } yield keyPair.asRight
      else
        Sync[F].pure(
          Failure
            .json(
              StatusCode.Forbidden,
              IgluResponse.Message(s"Cannot create ${vendorPrefix.show} using your permissions")
            )
            .asLeft
        )
    } else
      Sync[F].pure(
        Failure.json(StatusCode.Forbidden, IgluResponse.Message("Not sufficient privileges to create keys")).asLeft
      )
}
