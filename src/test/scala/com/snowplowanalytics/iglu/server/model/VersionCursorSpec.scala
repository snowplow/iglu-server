/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.0
 * located at https://docs.snowplow.io/limited-use-license-1.0
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */

package com.snowplowanalytics.iglu.server.model

import com.snowplowanalytics.iglu.core.SchemaVer
import com.snowplowanalytics.iglu.server.model.VersionCursor.Inconsistency

class VersionCursorSpec extends org.specs2.Specification {
  def is = s2"""
  isVersionAllowed validates new revision $e1
  isVersionAllowed validates new model if no schemas were created for this model yet $e2
  isVersionAllowed rejects new model if previous model does not exist yet $e3
  isVersionAllowed validates new addition $e4
  isVersionAllowed rejects new addition if previous does not exist $e5
  isAllowed allows overriding schema if patchesAllowed set to true $e6
  isAllowed forbids overriding schema if patchesAllowed set to false $e7
  isAllowed forbids superseding nonexistent schema versions $e8
  isAllowed forbids superseding superior schema versions $e9
  isVersionAllowed rejects new addition if next revision exists $e10
  """

  def e1 = {
    val existing = List(SchemaVer.Full(1, 0, 0), SchemaVer.Full(1, 0, 1))
    val current  = VersionCursor.get(SchemaVer.Full(1, 1, 0))
    VersionCursor.isVersionAllowed(existing, current) must beRight
  }

  def e2 = {
    val existing = List(SchemaVer.Full(1, 0, 0), SchemaVer.Full(1, 1, 0), SchemaVer.Full(1, 0, 1))
    val current  = VersionCursor.get(SchemaVer.Full(2, 0, 0))
    VersionCursor.isVersionAllowed(existing, current) must beRight
  }

  def e3 = {
    val existing = List(SchemaVer.Full(1, 0, 0), SchemaVer.Full(1, 1, 0))
    val current  = VersionCursor.get(SchemaVer.Full(3, 0, 0))
    VersionCursor.isVersionAllowed(existing, current) must beLeft
  }

  def e4 = {
    val existing = List(SchemaVer.Full(1, 0, 0), SchemaVer.Full(1, 1, 0), SchemaVer.Full(1, 1, 1))
    val current  = VersionCursor.get(SchemaVer.Full(1, 1, 2))
    VersionCursor.isVersionAllowed(existing, current) must beRight
  }

  def e5 = {
    val existing = List(SchemaVer.Full(1, 0, 0), SchemaVer.Full(1, 1, 0), SchemaVer.Full(1, 1, 1))
    val current  = VersionCursor.get(SchemaVer.Full(1, 1, 3))
    VersionCursor.isVersionAllowed(existing, current) must beLeft
  }

  def e6 =
    VersionCursor.isAllowed(SchemaVer.Full(1, 0, 0), List(SchemaVer.Full(1, 0, 0)), true, List.empty) must beRight(())

  def e7 =
    VersionCursor.isAllowed(SchemaVer.Full(1, 0, 0), List(SchemaVer.Full(1, 0, 0)), false, List.empty) must beLeft(
      VersionCursor.Inconsistency.AlreadyExists
    )

  def e8 =
    VersionCursor.isAllowed(
      SchemaVer.Full(2, 0, 0),
      List(SchemaVer.Full(1, 0, 0)),
      false,
      List(SchemaVer.Full(1, 0, 1))
    ) must beLeft(
      VersionCursor.Inconsistency.SupersededMissing
    )

  def e9 =
    VersionCursor.isAllowed(
      SchemaVer.Full(1, 0, 1),
      List(SchemaVer.Full(1, 0, 0), SchemaVer.Full(2, 0, 0)),
      false,
      List(SchemaVer.Full(2, 0, 0))
    ) must beLeft(
      VersionCursor.Inconsistency.SupersededInvalid
    )

  def e10 = {
    val existing = List(SchemaVer.Full(1, 0, 0), SchemaVer.Full(1, 1, 0))
    val current  = VersionCursor.get(SchemaVer.Full(1, 0, 1))
    VersionCursor.isVersionAllowed(existing, current) must beLeft(Inconsistency.NextRevisionExists)
  }
}
