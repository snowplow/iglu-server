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

import java.util.UUID
import java.time.Instant
import cats.effect.{Clock, IO}
import fs2.Stream
import io.circe.Json
import io.circe.literal._
import org.http4s._
import org.http4s.rho.AuthedContext
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer}
import com.snowplowanalytics.iglu.server.model.Permission.{SchemaAction, Vendor}
import com.snowplowanalytics.iglu.server.model.Schema.SupersedingInfo
import model.{Permission, Schema, SchemaDraft}
import storage.InMemory

object SpecHelpers {

  implicit val C = Clock.create[IO]

  val ctx = new AuthedContext[IO, Permission]

  val now         = Instant.ofEpochMilli(1537621061000L)
  val superKey    = UUID.fromString("4ed2d87a-6da5-48e8-a23b-36a26e61f974")
  val readKey     = UUID.fromString("1eaad173-1da5-eef8-a2cb-3fa26e61f975")
  val readKeyAcme = UUID.fromString("2abad125-0ba1-faf2-b2cc-4fa26e61f971")

  val schemaZero = json"""{"type": "object", "properties": {"one": {}}}"""
  val selfSchemaZero =
    json"""{"$$schema" : "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#", "type": "object", "properties": {"one": {}}, "self": {"vendor" : "com.acme", "name" : "event", "format" : "jsonschema", "version" : "1-0-0"}}"""
  val schemaOne     = json"""{"type": "object", "properties": {"one": {}, "two": {}}}"""
  val schemaPrivate = json"""{"type": "object", "properties": {"password": {}}, "required": ["password"]}"""
  val selfSchemaPrivate =
    json"""{"$$schema" : "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#", "type": "object", "properties": {"password": {}}, "required": ["password"], "self": {"vendor" : "com.acme", "name" : "secret", "format" : "jsonschema", "version" : "1-0-0"}}"""

  // exampleState content
  val schemas = List(
    // public schemas
    Schema(
      SchemaMap("com.acme", "event", "jsonschema", SchemaVer.Full(1, 0, 0)),
      Schema.Metadata(now, now, true),
      schemaZero,
      SupersedingInfo.empty
    ),
    Schema(
      SchemaMap("com.acme", "event", "jsonschema", SchemaVer.Full(1, 0, 1)),
      Schema.Metadata(now, now, true),
      schemaOne,
      SupersedingInfo.empty
    ),
    // private
    Schema(
      SchemaMap("com.acme", "secret", "jsonschema", SchemaVer.Full(1, 0, 0)),
      Schema.Metadata(now, now, false),
      schemaPrivate,
      SupersedingInfo.empty
    )
  ).map(s => (s.schemaMap, s)).toMap

  val drafts = List.empty[SchemaDraft].map(d => (d.schemaMap, d)).toMap

  val exampleState = InMemory.State(
    schemas,
    Map.empty,
    Map(
      superKey    -> Permission.Super,
      readKey     -> Permission.ReadOnlyAny,
      readKeyAcme -> Permission(Vendor(List("com", "acme"), false), Some(SchemaAction.Read), Set.empty)
    ),
    drafts
  )

  def toBytes(entity: Json) =
    Stream.emits(entity.noSpaces.stripMargin.getBytes).covary[IO]

  implicit class SchemaKeyUri(schemaKey: SchemaKey) {
    def uri: Uri = Uri.unsafeFromString(schemaKey.toPath)
  }
}
