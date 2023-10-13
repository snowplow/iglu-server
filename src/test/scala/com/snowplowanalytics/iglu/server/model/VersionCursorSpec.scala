/*
 * Copyright (c) 2019-2023 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.server.model

import com.snowplowanalytics.iglu.core.SchemaVer
import com.snowplowanalytics.iglu.server.model.VersionCursor.Inconsistency

class VersionCursorSpec extends org.specs2.Specification {
  def is = s2"""
  previousExists validates new revision $e1
  previousExists validates new model if no schemas were created for this model yet $e2
  previousExists rejects new model if previous model does not exist yet $e3
  previousExists validates new addition $e4
  previousExists rejects new addition if previous does not exist $e5
  isAllowed allows overriding schema if patchesAllowed set to true $e6
  isAllowed forbids overriding schema if patchesAllowed set to false $e7
  previousExists rejects new addition if next revision exists $e8
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
    VersionCursor.isAllowed(SchemaVer.Full(1, 0, 0), List(SchemaVer.Full(1, 0, 0)), true) must beRight(())

  def e7 =
    VersionCursor.isAllowed(SchemaVer.Full(1, 0, 0), List(SchemaVer.Full(1, 0, 0)), false) must beLeft(
      VersionCursor.Inconsistency.AlreadyExists
    )

  def e8 = {
    val existing = List(SchemaVer.Full(1, 0, 0), SchemaVer.Full(1, 1, 0))
    val current  = VersionCursor.get(SchemaVer.Full(1, 0, 1))
    VersionCursor.isVersionAllowed(existing, current) must beLeft(Inconsistency.NextRevisionExists)
  }
}
