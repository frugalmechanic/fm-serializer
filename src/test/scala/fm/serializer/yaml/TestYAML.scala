/*
 * Copyright 2014 Frugal Mechanic (http://frugalmechanic.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fm.serializer.yaml

import fm.serializer.{Deserializer, Field, Serializer}

final class TestDefaultYAML extends TestYAML {
  def serialize[T](v: T)(implicit ser: Serializer[T]): String = YAML.toYAML[T](v)

  test("String Map - Treated as object") {
    val map: Map[String,Int] = Map("foo" -> 123, "bar" -> 321)
    val yaml: String = serialize(map)

    yaml should equal (
      """|foo: 123,
         |bar: 321""".stripMargin)

    deserialize[Map[String,Int]](yaml) should equal (map)
  }

  test("Int Map - Treated as array of tuples") {
    val map: Map[Int,String] = Map(123 -> "foo", 312 -> "bar")
    val yaml: String = serialize(map)

    yaml should equal (
      """|[
         | - _1: 123
         |   _2: foo
         | - _1: 312
         |   _2: bar
         |]""".stripMargin)

    deserialize[Map[Int,String]](yaml) should equal (map)
  }

  test("Boolean types") {
    case class BooleanKey(key: Boolean)
    deserialize[BooleanKey]("""key: yes""") should equal(BooleanKey(true))
    deserialize[BooleanKey]("""key: Yes""") should equal(BooleanKey(true))
    deserialize[BooleanKey]("""key: YES""") should equal(BooleanKey(true))

    deserialize[BooleanKey]("""key: true""") should equal(BooleanKey(true))
    deserialize[BooleanKey]("""key: True""") should equal(BooleanKey(true))
    deserialize[BooleanKey]("""key: TRUE""") should equal(BooleanKey(true))

    deserialize[BooleanKey]("""key: no""") should equal(BooleanKey(false))
    deserialize[BooleanKey]("""key: No""") should equal(BooleanKey(false))
    deserialize[BooleanKey]("""key: NO""") should equal(BooleanKey(false))

    deserialize[BooleanKey]("""key: false""") should equal(BooleanKey(false))
    deserialize[BooleanKey]("""key: False""") should equal(BooleanKey(false))
    deserialize[BooleanKey]("""key: FALSE""") should equal(BooleanKey(false))
  }

  case class Unquoted(name: String, int: Int, long: Long)

  test("Unquoted Field Names") {
    deserialize[Unquoted](
      """|name: null
         |int: 123
         |long: 123123123123123""".stripMargin) should equal(Unquoted(null, 123, 123123123123123L))

    deserialize[Unquoted](
      """|name: nullnot
         |int: 123
         |long: 123123123123123""".stripMargin) should equal(Unquoted("nullnot", 123, 123123123123123L))

    deserialize[Unquoted](
      """|name: foo
         |int: 123
         |long: 123123123123123""".stripMargin) should equal(Unquoted("foo", 123, 123123123123123L))

    deserialize[Unquoted](
      """|name: foo
         |int: "123"
         |long: "123123123123123"""".stripMargin) should equal(Unquoted("foo", 123, 123123123123123L))
  }

  case class AlternateName(@Field("type") tpe: String, @Field foo: Int)

  test("Alternate Field Name") {
    val instance: AlternateName = AlternateName("the_type_field", 123)
    val yaml: String =
      """|"type": "the_type_field"
         |"foo": 123""".stripMargin

    serialize(instance) should equal (yaml)
    deserialize[AlternateName](yaml) should equal (instance)
  }
}


final class TestMinimalYAML extends TestYAML {
  def serialize[T](v: T)(implicit ser: Serializer[T]): String = YAML.toMinimalYAML[T](v)
  override def ignoreNullRetainTest: Boolean = true
}

final class TestPrettyYAML extends TestYAML {
  def serialize[T](v: T)(implicit ser: Serializer[T]): String = YAML.toPrettyYAML[T](v)
}

abstract class TestYAML extends fm.serializer.TestSerializer[String]  {
  def serialize[T](v: T)(implicit ser: Serializer[T]): String
  final def deserialize[T](yaml: String)(implicit deser: Deserializer[T]): T = YAML.fromYAML[T](yaml)
}