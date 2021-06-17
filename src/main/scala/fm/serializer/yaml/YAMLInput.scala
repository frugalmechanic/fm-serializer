/*
 * Copyright 2019 Frugal Mechanic (http://frugalmechanic.com)
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

import fm.serializer.{CollectionInput, FieldInput, Input}
import fm.common.Implicits._
import fm.common.Logging
import org.yaml.snakeyaml.events._
import fm.serializer.base64.Base64
import org.yaml.snakeyaml.nodes.Tag

// (Correct) YAML Uses Spaces for indentation: https://yaml.org/spec/1.1/#id871998
abstract class YAMLInput(options: YAMLOptions) extends Input with Logging {
  def allowStringMap: Boolean = true

  //
  // HELPERS
  //

  protected def debug(): Unit

  protected def hasNext(): Boolean

  /** Current Event, does not consume it */
  protected def headOption(): Option[Event]
  private def head: Event = headOption.get

  /** Return peek and advance to the next event */
  protected def next(): Event

  /** Checks for end of document or end of stream */
  protected def isEOF(event: Event): Boolean

  /** Verifies the type and reads to next event */
  protected def readNextRequired(tpe: Event.ID): Unit

  /*
  private def readTag(): String = {
    head match {
      case scalar: ScalarEvent              => scalar.getTag
      case collection: CollectionStartEvent => collection.getTag
      case map: MappingStartEvent           => map.getTag
      //case node: NodeEvent                  => node.getTag
      case e: Event                         => throw new IllegalStateException(s"Expected to read an event with a tag, but got Event: $e")
    }
  }

  private def readAnchor(): String = {
    head match {
      case scalar: ScalarEvent              => scalar.getAnchor
      case collection: CollectionStartEvent => collection.getAnchor
      case map: MappingStartEvent           => map.getAnchor
      //case node: NodeEvent                  => node.getTag
      case e: Event                         => throw new IllegalStateException(s"Expected to read an event with a tag, but got Event: $e")
    }
  }
  */

  private def isEOFOrMappingEnd(event: Event): Boolean = isEOF(event) || event.is(Event.ID.MappingEnd)

  def setIsChildren(): Unit

  private def isEndMarker(event: Event): Boolean = {
    isEOFOrMappingEnd(event) || event.is(Event.ID.SequenceEnd)
  }

  def readFieldName(): String = {
    logger.error(s"readFieldName!")

    if (nextValueIsNull) return null

    val ret: String = head match {
      case scalar: ScalarEvent              => scalar.getValue
      //case node: NodeEvent                  => node.getTag
      case e: Event                         => throw new IllegalStateException(s"Expected to read an event with a tag, but got Event: $e")
    }


    next()

    logger.error(s"readFieldName(): ${ret}, next is: $head")
    ret
  }

  //
  // FIELD Input
  //
  def readFieldNumber(nameToNumMap: Map[String, Int]): Int = {
    logger.error(s"readFieldNumber!")
    val name: String = readFieldName()
    
    if (null == name) return 0
    
    try {
      // Exact name match
      nameToNumMap(name)
    } catch {
      case _: NoSuchElementException =>
        // TODO: possibly require that the map be pre-populated with the lower case versions so we don't have to search through it
        val lowerName: String = name.toLowerCase
        if (nameToNumMap.contains(lowerName)) nameToNumMap(lowerName)
        else nameToNumMap.find{ case (n,i) => n.toLowerCase == lowerName }.map{ _._2 }.getOrElse(-1)
    }
  }


  def skipUnknownField(): Unit = {
    logger.error(s"skipUnknownField!")
    require(headOption.isDefined, "Expected headOption")
    head match {
      case e: ScalarEvent          => readRawString()
      case e: MappingStartEvent    => readRawObject(skipRawObjectFun)
      case e: SequenceStartEvent   => readRawCollection(skipRawCollectionFun)
      case e: Event                => throw new IllegalStateException(s"Trying to skip an unknown field, no handler for event: $e")
    }
  }
  
  // This avoids the object creations in skipUnknownField()
  private[this] val skipRawObjectFun: Function1[FieldInput, AnyRef] = new Function1[FieldInput, AnyRef] {
    def apply(in: FieldInput): AnyRef = {
      while(in.readFieldNumber(Map.empty) != 0) in.skipUnknownField()
      null
    }
  }
  
  // This avoids the object creations in skipUnknownField()
  private[this] val skipRawCollectionFun: Function1[CollectionInput, AnyRef] = new Function1[CollectionInput, AnyRef] {
    def apply(in: CollectionInput): AnyRef = {
      while(in.hasAnotherElement) skipUnknownField()
      null
    }
  }

  //
  // COLLECTION Input
  //
  def hasAnotherElement: Boolean = head.is(Event.ID.Scalar)
  
  //
  // RAW Input
  //

  private def readStringValue(): String = {
    logger.error(s"readStringValue(), nextValueIsNull: ${nextValueIsNull}")

    if (nextValueIsNull) null
    else {
      val ret = head match {
        case scalar: ScalarEvent => scalar.getValue
        case e: Event            => throw new IllegalStateException(s"Expected to read a scalar value, but read: $e")
      }

      next()

      logger.error(s"readStringValue() ret: $ret and next: $head")
      ret
    }
  }
  
  // Basic Types
  def readRawBool(): Boolean  = readStringValue().parseBoolean.get
  def readRawFloat(): Float   = readStringValue().toFloat
  def readRawDouble(): Double = readStringValue().toDouble
  
  def readRawString(): String = {
    if (options.internStrings) readStringValue().intern() else readStringValue()
  }
  
  // Bytes
  def readRawByteArray(): Array[Byte] = {
    val s: String = readStringValue()
    if (null == s) null else Base64.decode(s)
  }
  
  // Ints  
  def readRawInt(): Int = readStringValue().toInt
  def readRawUnsignedInt(): Int = readRawInt()
  def readRawSignedInt(): Int = readRawInt()
  def readRawFixedInt(): Int = readRawInt()
  
  // Longs
  def readRawLong(): Long = readStringValue().toLong
  def readRawUnsignedLong(): Long = readRawLong()
  def readRawSignedLong(): Long = readRawLong()
  def readRawFixedLong(): Long = readRawLong()

  def nextValueIsNull: Boolean = {
    headOption.exists{ isEndMarker } || (head match {
      case scalar: ScalarEvent =>
        val ret: Boolean = Tag.NULL.equals(scalar.getTag) || scalar.getValue.isNullOrBlank || (scalar.getValue === "null")
        if (ret) next()
        ret
      //case beginMapping: MappingStartEvent => true
      //case beginCollection: SequenceStartEvent => true
      case event: Event        => false
    })
  }

  // Objects
  def readRawObject[T](f: FieldInput => T): T = {
    logger.error("readRawObject!")

    if (nextValueIsNull) return null.asInstanceOf[T]

    readNextRequired(Event.ID.MappingStart)

    logger.error("readRawObject, after mapping start check")

    if (head.is(Event.ID.MappingEnd)) {
      next()
      null.asInstanceOf[T]
    } else {
      val res: T = f(this)
      logger.error(s"readRawObject after mapping start. head now: $head, and ${res}")
      readNextRequired(Event.ID.MappingEnd)

      res
    }
  }
  
  // Collections
  def readRawCollection[T](f: CollectionInput => T): T = {
    logger.error(s"readRawCollection. $head, nextValueIsNull: ${nextValueIsNull}")

    if (nextValueIsNull/* && !head.is(Event.ID.SequenceStart)*/) return null.asInstanceOf[T]


    readNextRequired(Event.ID.SequenceStart)

    logger.error(s"readRawCollection after sequence start. $head")
    if (head.is(Event.ID.SequenceEnd)) {
      next()
      null.asInstanceOf[T]
    } else {
      val res: T = f(this)

      logger.error(s"readRawCollection after sequence start. head now: $head, and ${res}, ${f}")
      readNextRequired(Event.ID.SequenceEnd)

      res
    }
  }
  
  //
  // NESTED Input
  //
  
  // Basic Types
  def readNestedBool(): Boolean = readRawBool()
  
  def readNestedFloat(): Float = readRawFloat()
  
  def readNestedDouble(): Double = readRawDouble()
  
  def readNestedString(): String = readRawString()
  
  // Bytes
  def readNestedByteArray(): Array[Byte] = readRawByteArray()
  
  // Ints  
  def readNestedInt(): Int = readRawInt()
  def readNestedUnsignedInt(): Int = readNestedInt()
  def readNestedSignedInt(): Int = readNestedInt()
  def readNestedFixedInt(): Int = readNestedInt()
  
  // Longs
  def readNestedLong(): Long = readRawLong()
  def readNestedUnsignedLong(): Long = readNestedLong()
  def readNestedSignedLong(): Long = readNestedLong()
  def readNestedFixedLong(): Long = readNestedLong()
    
  // Objects
  def readNestedObject[T](f: FieldInput => T): T = {
    logger.error(s"readNestedObject")
    readRawObject(f)
  }
  
  // Collections
  def readNestedCollection[T](f: CollectionInput => T): T = {
    logger.error(s"readNestedCollection")
    readRawCollection(f)
  }
}
