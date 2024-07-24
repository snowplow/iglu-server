/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.model

import java.time.Instant

import cats.implicits._

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto._

import doobie._
import doobie.postgres.circe.json.implicits._
import doobie.postgres.implicits._

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.implicits._

import Schema.Metadata

case class Schema(
  schemaMap: SchemaMap,
  metadata: Metadata,
  body: Json,
  supersedingInfo: Schema.SupersedingInfo
) {
  def withFormat(repr: Schema.Repr.Format): Schema.Repr = repr match {
    case Schema.Repr.Format.Canonical =>
      Schema.Repr.Canonical(canonical, supersedingInfo)
    case Schema.Repr.Format.Uri =>
      Schema.Repr.Uri(schemaMap.schemaKey)
    case Schema.Repr.Format.Meta =>
      Schema.Repr.Full(this)
  }

  def canonical: SelfDescribingSchema[Json] =
    SelfDescribingSchema(schemaMap, body)

}

object Schema {

  val CanonicalUri      = "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#"
  val SupersededByField = "$supersededBy"
  val SupersedesField   = "$supersedes"

  case class Metadata(createdAt: Instant, updatedAt: Instant, isPublic: Boolean)

  object Metadata {
    implicit val metadataEncoder: Encoder[Metadata] =
      deriveEncoder[Metadata]

    implicit val metadataDecoder: Decoder[Metadata] =
      deriveDecoder[Metadata]

  }

  /** Encoding of a schema */
  sealed trait Repr
  object Repr {

    /** Canonical self-describing representation */
    case class Canonical(schema: SelfDescribingSchema[Json], supersedingInfo: SupersedingInfo) extends Repr

    /** Non-vanilla representation for UIs/non-validation clients */
    case class Full(schema: Schema) extends Repr

    /** Just URI string (but schema is on the server) */
    case class Uri(schemaKey: SchemaKey) extends Repr

    def apply(schema: Schema): Repr = Full(schema)
    def apply(uri: SchemaMap): Repr = Uri(uri.schemaKey)
    def apply(schema: SelfDescribingSchema[Json], supersedingInfo: SupersedingInfo): Repr =
      Canonical(schema, supersedingInfo)

    sealed trait Format extends Product with Serializable
    object Format {
      case object Uri       extends Format
      case object Meta      extends Format
      case object Canonical extends Format

      def parse(s: String): Option[Format] = s.toLowerCase match {
        case "uri"       => Some(Uri)
        case "meta"      => Some(Meta)
        case "canonical" => Some(Canonical)
        case _           => None
      }
    }

  }

  sealed trait SchemaBody extends Product with Serializable
  object SchemaBody {
    case class SelfDescribing(schema: SelfDescribingSchema[Json], supersedingInfo: SupersedingInfo) extends SchemaBody
    case class BodyOnly(schema: Json, supersedingInfo: SupersedingInfo)                             extends SchemaBody

    implicit val schemaBodyCirceDecoder: Decoder[SchemaBody] =
      Decoder.instance { cursor =>
        for {
          removed         <- SupersedingInfo.removeSupersedingInfoFields(cursor)
          supersedingInfo <- cursor.as[SupersedingInfo]
          res <- SelfDescribingSchema.parse(removed) match {
            case Right(schema) => SelfDescribing(schema, supersedingInfo).asRight
            case Left(_)       => removed.as[JsonObject].map(obj => BodyOnly(Json.fromJsonObject(obj), supersedingInfo))
          }
        } yield res
      }
  }

  case class SupersedingInfo(supersededBy: Option[SchemaVer.Full], supersedes: List[SchemaVer.Full])

  object SupersedingInfo {
    def empty = SupersedingInfo(Option.empty, List.empty)

    implicit val supersedingInfoDecoder: Decoder[SupersedingInfo] =
      Decoder.instance { json =>
        for {
          supersededBy <- json.getOrElse[Option[SchemaVer.Full]](SupersededByField)(None)
          supersedes   <- json.getOrElse[List[SchemaVer.Full]](SupersedesField)(List.empty)
        } yield SupersedingInfo(supersededBy, supersedes)
      }

    implicit val supersedingInfoEncoder: Encoder[SupersedingInfo] =
      Encoder.instance { info =>
        Json
          .obj(
            SupersededByField -> info.supersededBy.map(_.asString).asJson,
            SupersedesField   -> info.supersedes.map(_.asString).asJson
          )
          .dropNullValues
          .dropEmptyValues
      }

    def removeSupersedingInfoFields(json: HCursor) =
      for {
        obj <- json.as[JsonObject]
        removed = obj.remove(SupersededByField).remove(SupersedesField)
      } yield Json.fromJsonObject(removed)
  }

  sealed trait Format extends Product with Serializable
  object Format {
    case object Jsonschema extends Format

    def parse(s: String): Option[Format] = s match {
      case "jsonschema" => Some(Jsonschema)
      case _            => None
    }
  }

  private def moveToFront[K, V](keys: List[K], fields: List[(K, V)]): List[(K, V)] =
    keys match {
      case h :: t =>
        fields.span(_._1 != h) match {
          case (previous, matches :: next) => matches :: moveToFront(t, previous ++ next)
          case _                           => moveToFront(t, fields)
        }
      case Nil => fields
    }

  private def orderedSchema(schema: Json): Json =
    schema.asObject match {
      case Some(obj) =>
        val frontKeys = List(s"$$schema", SupersededByField, SupersedesField, "self", "metadata")
        Json.fromFields(moveToFront(frontKeys, obj.toList))
      case None => schema
    }

  implicit val schemaEncoder: Encoder[Schema] =
    Encoder.instance { schema =>
      Json
        .obj(
          "self"     -> schema.schemaMap.asJson,
          "metadata" -> schema.metadata.asJson(Metadata.metadataEncoder)
        )
        .deepMerge(schema.supersedingInfo.asJson)
        .deepMerge(schema.body)
    }

  implicit val representationEncoder: Encoder[Repr] =
    Encoder.instance {
      case Repr.Full(s) => orderedSchema(schemaEncoder.apply(s))
      case Repr.Uri(u)  => Encoder[String].apply(u.toSchemaUri)
      case Repr.Canonical(schema, supersedingInfo) =>
        orderedSchema {
          Json
            .obj(
              s"$$schema" -> CanonicalUri.asJson
            )
            .deepMerge(supersedingInfo.asJson)
            .deepMerge(schema.normalize)
        }
    }

  implicit val serverSchemaDecoder: Decoder[Schema] =
    Decoder.instance { cursor =>
      for {
        self     <- cursor.value.as[SchemaMap]
        meta     <- cursor.downField("metadata").as[Metadata]
        bodyJson <- cursor.as[JsonObject]
        body = bodyJson.toList.filterNot {
          case (key, _) => List("self", "metadata", SupersededByField, SupersedesField).contains(key)
        }
        supersedingInfo <- cursor.value.as[SupersedingInfo]
      } yield Schema(self, meta, Json.fromFields(body), supersedingInfo)
    }

  implicit val schemaVerFull: Read[Option[SchemaVer.Full]] =
    Read[Option[String]].map(_.flatMap(v => SchemaVer.parseFull(v).toOption))

  implicit val schemaVerFullList: Read[List[SchemaVer.Full]] =
    Read[Option[List[String]]].map(_.fold(List.empty[SchemaVer.Full]) {
      _.flatMap(v => SchemaVer.parseFull(v).toOption)
    })

  implicit val schemaDoobieRead: Read[Schema] =
    Read[(SchemaMap, Metadata, Json, Option[SchemaVer.Full], List[SchemaVer.Full])].map {
      case (schemaMap, meta, body, supersededBy, supersedes) =>
        Schema(schemaMap, meta, body, SupersedingInfo(supersededBy, supersedes))
    }
}
