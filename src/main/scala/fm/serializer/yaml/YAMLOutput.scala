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

import fm.common.Logging
import fm.common.Implicits._
import fm.serializer.{FieldOutput, NestedOutput, Output}
import java.util.{ArrayList => JavaArrayList}
import org.yaml.snakeyaml.representer.Representer
import org.yaml.snakeyaml.nodes._
import org.yaml.snakeyaml.serializer.{Serializer => YAMLSerializer}
import org.yaml.snakeyaml.DumperOptions

object YAMLOutput {
  private val representer: Representer = {
    val res: Representer = new Representer()

    // Not exactly sure why these defaults don't get picked up without being explicit here...
    res.setDefaultScalarStyle(DumperOptions.ScalarStyle.PLAIN)
    res.setDefaultFlowStyle(DumperOptions.FlowStyle.AUTO)

    res
  }

  private def NullNode(): Node = representer.represent(null)
    //new ScalarNode(Tag.NULL, "null", null, null, DumperOptions.ScalarStyle.PLAIN)

  private case class JavaArrayListStack[T]() extends SimpleStack[JavaArrayList[T]] {
    def createNewHead: JavaArrayList[T] = {
      val newHead: JavaArrayList[T] = new JavaArrayList[T]()
      push(newHead)
      newHead
    }
  }

  private case class ObjectStack() extends SimpleStack[MappingNode]

  // 2.12 scaladocs says Stack is deprecated and you should just use a List *shrug*
  private sealed trait SimpleStack[T] {
    // This is a pseudo-stack for keeping track of potential nested-arrays, in a FIFO List order.
    // The complete Node (with children) must be created before we can do a writer.serialize(node: SequenceNode)
    private var _stack: List[T] = List.empty[T]

    def head(): T = _stack.head

    def push(newHead: T): Unit = {
      _stack = newHead +: _stack
    }

    def pop(): Unit = {
      require(_stack.nonEmpty, s"Attempted to pop ${this}, but it's empty!")
      _stack = _stack.tail
    }

    def isEmpty(): Boolean = _stack.isEmpty
    def nonEmpty(): Boolean = _stack.nonEmpty

    override def toString(): String = s"${this.getClass.toString}(${_stack.toString})"
  }
}

final class YAMLOutput(writer: YAMLSerializer, options: DumperOptions) extends Output with Logging {
  import YAMLOutput._

  def allowStringMap: Boolean = true

  // we can't stream nested nodes, so

  // This is a pseudo-stack for keeping track of potential nested-arrays, in a FIFO List order.
  // The complete Node (with children) must be created before we can do a writer.serialize(node: SequenceNode) or writer.serialize(node: MappingNode)
  private val collectionStack: JavaArrayListStack[Node] = JavaArrayListStack[Node]()
  private val mapChildrenStack: JavaArrayListStack[NodeTuple] = JavaArrayListStack[NodeTuple]()

  implicit def toNodeValue[T <: AnyVal](value: T): Node = representer.represent(value)
  implicit def toNodeValue(value: String): Node = representer.represent(value)
  implicit def toNodeValue(value: Array[Byte]): Node = representer.represent(value)

  //
  // Inherited from RawOutput, these should only be used to write elements into a Collection
  //

  // Basic Types

  private def addValueToCollection(value: Node): Unit = collectionStack.head.add(value)

  def writeRawBool(value: Boolean): Unit = addValueToCollection(value)
  def writeRawFloat(value: Float): Unit = addValueToCollection(value)
  def writeRawDouble(value: Double): Unit = addValueToCollection(value)
  def writeRawString(value: String): Unit = addValueToCollection(value)
  def writeRawByteArray(value: Array[Byte]): Unit = addValueToCollection(value)

  // Ints
  def writeRawInt(value: Int): Unit = addValueToCollection(value)
  def writeRawUnsignedInt(value: Int): Unit = addValueToCollection(value)
  def writeRawSignedInt(value: Int): Unit = addValueToCollection(value)
  def writeRawFixedInt(value: Int): Unit = addValueToCollection(value)

  // Longs
  def writeRawLong(value: Long): Unit = addValueToCollection(value)
  def writeRawUnsignedLong(value: Long): Unit = addValueToCollection(value)
  def writeRawSignedLong(value: Long): Unit = addValueToCollection(value)
  def writeRawFixedLong(value: Long): Unit = addValueToCollection(value)

  /**
    * For writing objects.  Note: that the obj is passed in for null handling
    * by the implementation.  If the object is not null then the function f
    * will be called so the caller can write out the fields
    */
  def writeRawObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    require(
      (
        (mapChildrenStack.isEmpty() && collectionStack.nonEmpty()) ||
        (mapChildrenStack.nonEmpty() === collectionStack.nonEmpty())
      ),
      s"""|writeRawObject($obj) but mapChildrenStack and collectionStack conflict.:
          |
          |mapChildren.isEmpty && collectionStack.nonEmpty: ${(mapChildrenStack.isEmpty() && collectionStack.nonEmpty())}
          |mapChildren.nonEmpty && collectionStack.nonEmpty: ${(mapChildrenStack.nonEmpty() && collectionStack.nonEmpty())}
          |mapChildren.isEmpty && collectionStack.isEmpty: ${(mapChildrenStack.isEmpty() && collectionStack.isEmpty())}
          |
          |mapChildrenStack:
          |
          |$mapChildrenStack
          |
          |collectionStack:
          |
          |$collectionStack""".stripMargin
    )

    if (null == obj) {
      if (collectionStack.nonEmpty()) addValueToCollection(NullNode)
      else writer.serialize(NullNode)
    } else {
      val mapChildren: JavaArrayList[NodeTuple] = mapChildrenStack.createNewHead
      val mapNode: MappingNode = new MappingNode(Tag.MAP, mapChildren, options.getDefaultFlowStyle)

      // Nested objects require creating the complete object Node so we can add it to the collection,
      // and finish writing the enclosing yaml array
      f(this, obj)

      mapChildrenStack.pop

      if (collectionStack.nonEmpty()) addValueToCollection(mapNode)
      else writer.serialize(mapNode)
    }
  }


  def writeRawCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = {
    require(
      (
        (mapChildrenStack.isEmpty() && collectionStack.nonEmpty()) ||
        (mapChildrenStack.nonEmpty() === collectionStack.nonEmpty())
      ),
      s"""|writeRawCollection($col) but mapChildrenStack and collectionStack conflict:
          |
          |mapChildren.isEmpty && collectionStack.nonEmpty: ${(mapChildrenStack.isEmpty() && collectionStack.nonEmpty())}
          |mapChildren.nonEmpty == collectionStack.nonEmpty: ${(mapChildrenStack.nonEmpty() === collectionStack.nonEmpty())}
          |
          |mapChildrenStack:
          |
          |$mapChildrenStack
          |
          |collectionStack:
          |
          |${collectionStack}""".stripMargin
    )

    if (null == col) {
      if (collectionStack.nonEmpty) addValueToCollection(NullNode)
      else writer.serialize(NullNode)
    } else {
      val seqChildren: JavaArrayList[Node] = collectionStack.createNewHead
      val seqNode: SequenceNode = new SequenceNode(Tag.SEQ, seqChildren, options.getDefaultFlowStyle)

      f(this, col)

      collectionStack.pop

      if (collectionStack.nonEmpty) addValueToCollection(seqNode)
      else writer.serialize(seqNode)
    }
  }

  //
  // Inherited from NestedOutput
  //

  // Basic Types
  def writeNestedBool(value: Boolean): Unit = writeRawBool(value)
  def writeNestedFloat(value: Float): Unit = writeRawFloat(value)
  def writeNestedDouble(value: Double): Unit = writeRawDouble(value)
  def writeNestedString(value: String): Unit = writeRawString(value)

  // Bytes
  def writeNestedByteArray(value: Array[Byte]): Unit = writeRawByteArray(value)

  // Ints
  def writeNestedInt(value: Int): Unit = writeRawInt(value)
  def writeNestedUnsignedInt(value: Int): Unit = writeRawUnsignedInt(value)
  def writeNestedSignedInt(value: Int): Unit = writeRawSignedInt(value)
  def writeNestedFixedInt(value: Int): Unit = writeRawFixedInt(value)

  // Longs
  def writeNestedLong(value: Long): Unit = writeRawLong(value)
  def writeNestedUnsignedLong(value: Long): Unit = writeRawUnsignedLong(value)
  def writeNestedSignedLong(value: Long): Unit = writeRawSignedLong(value)
  def writeNestedFixedLong(value: Long): Unit = writeRawFixedLong(value)

  def writeNestedObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    writeRawObject(obj)(f)
  }
  def writeNestedCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = writeRawCollection(col)(f)

  //
  // Inherited from FieldOutput
  //

  private def writeField(keyNode: Node, valueNode: Node): Unit = {
    val nodeTuple = new NodeTuple(keyNode, valueNode)
    writeField(nodeTuple)
  }

  private def writeField(node: NodeTuple): Unit = {
    logger.error(s"writeField: $node")
    require(mapChildrenStack.nonEmpty, s"Attempted to write a NodeTuple ($node) but the MappingNode values stack is empty")
    mapChildrenStack.head.add(node)
  }

  // Basic Types
  def writeFieldBool(number: Int, name: String, value: Boolean): Unit = writeField(name, value)
  def writeFieldFloat(number: Int, name: String, value: Float): Unit = writeField(name, value)
  def writeFieldDouble(number: Int, name: String, value: Double): Unit = writeField(name, value)
  def writeFieldString(number: Int, name: String, value: String): Unit = writeField(name, value)

  // Bytes
  def writeFieldByteArray(number: Int, name: String, value: Array[Byte]): Unit = writeField(name, value)

  // Ints
  def writeFieldInt(number: Int, name: String, value: Int): Unit = writeField(name, value)
  def writeFieldUnsignedInt(number: Int, name: String, value: Int): Unit = writeField(name, value)
  def writeFieldSignedInt(number: Int, name: String, value: Int): Unit = writeField(name, value)
  def writeFieldFixedInt(number: Int, name: String, value: Int): Unit = writeField(name, value)

  // Longs
  def writeFieldLong(number: Int, name: String, value: Long): Unit = writeField(name, value)
  def writeFieldUnsignedLong(number: Int, name: String, value: Long): Unit = writeField(name, value)
  def writeFieldSignedLong(number: Int, name: String, value: Long): Unit = writeField(name, value)
  def writeFieldFixedLong(number: Int, name: String, value: Long): Unit = writeField(name, value)

  // Objects
  def writeFieldObject[T](number: Int, name: String, obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    require(mapChildrenStack.nonEmpty, s"writeFieldCollection($number, $name, $obj), but mapChildrenStack.isEmpty")

    logger.error(s"writeFieldObject($number, $name, $obj)")

    if (null == obj) {
      writeFieldNull(number, name)
    } else {
      val mapChildren: JavaArrayList[NodeTuple] = mapChildrenStack.createNewHead
      val mapNode: MappingNode = new MappingNode(Tag.MAP, mapChildren, options.getDefaultFlowStyle)

      f(this, obj)

      mapChildrenStack.pop

      writeField(name, mapNode)
    }
  }

  // Collections
  def writeFieldCollection[T](number: Int, name: String, col: T)(f: (NestedOutput, T) => Unit): Unit = {
    require(mapChildrenStack.nonEmpty, s"writeFieldCollection($number, $name, $col), but mapChildrenStack.isEmpty")

    logger.error(s"writeFieldCollection($number, $name, $col)")

    if (null == col) {
      logger.error(s"writeFieldCollection($number, $name, $col) col is null")
      writeFieldNull(number, name)
    } else {
      logger.error(s"writeFieldCollection($number, $name, $col) col is NOT null")
      val seqChildren: JavaArrayList[Node] = collectionStack.createNewHead
      val seqNode: SequenceNode = new SequenceNode(Tag.SEQ, seqChildren, options.getDefaultFlowStyle)

      f(this, col)

      collectionStack.pop

      writeField(name, seqNode)
    }
  }

  // Null
  def writeFieldNull(number: Int, name: String): Unit =  writeField(name, NullNode())
}