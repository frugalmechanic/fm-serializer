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

import fm.common.IOUtils
import fm.common.Logging
import fm.serializer.{Deserializer, Serializer}
import java.io._
import java.nio.charset.StandardCharsets.UTF_8
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.emitter.Emitter
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.resolver.Resolver
import org.yaml.snakeyaml.serializer.{Serializer => YAMLSerializer}

object YAML extends Logging {
  // TODO: Figure out if these can/should be tweaked for our output.
  private def makeDumperOptions(): DumperOptions = {
    val options: DumperOptions = new DumperOptions()

    options.setDefaultScalarStyle(DumperOptions.ScalarStyle.PLAIN)
    options.setDefaultFlowStyle(DumperOptions.FlowStyle.AUTO)

    options
  }

  private[this] val defaultDumperOptions = makeDumperOptions
  private[this] val minimalDumperOptions = makeDumperOptions
  private[this] val prettyDumperOptions = makeDumperOptions
  private[this] val defaultResolver = new Resolver()

  // writeToBsonWriter(v, new BsonBinaryWriter(buf))

  def toYAML[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toYAML(v, defaultDumperOptions)
  }

  def toMinimalYAML[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toYAML(v, minimalDumperOptions)
  }

  def toPrettyYAML[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toYAML(v, prettyDumperOptions)
  }

  def toYAML[@specialized T](v: T, options: DumperOptions)(implicit serializer: Serializer[T]): String = {
    val writer: StringWriter = new StringWriter()
    writeYAML(v, writer, options)
    writer.toString
  }

  def toBytes[@specialized T](v: T)(implicit serializer: Serializer[T]): Array[Byte] = {
    toBytes(v, defaultDumperOptions)
  }

  def toBytes[@specialized T](v: T, options: DumperOptions)(implicit serializer: Serializer[T]): Array[Byte] = {
    toYAML(v, options).getBytes(UTF_8)
  }

  def fromYAML[@specialized T](yaml: String)(implicit deserializer: Deserializer[T]): T = {
    fromYAML(yaml, YAMLOptions.default)
  }

  def fromYAML[@specialized T](yaml: String, options: YAMLOptions)(implicit deserializer: Deserializer[T]): T = {
    val input: YAMLReaderInput = new YAMLReaderInput(new StringReader(yaml), options)
    deserializer.deserializeRaw(input)
  }

  def fromBytes[@specialized T](bytes: Array[Byte])(implicit deserializer: Deserializer[T]): T = {
    fromBytes(bytes, YAMLOptions.default)
  }

  def fromBytes[@specialized T](bytes: Array[Byte], options: YAMLOptions)(implicit deserializer: Deserializer[T]): T = {
    val bis: ByteArrayInputStream = new ByteArrayInputStream(bytes)
    val charset: String = IOUtils.detectCharsetName(bis, useMarkReset = true).getOrElse(UTF_8.name)
    val reader: InputStreamReader = new InputStreamReader(bis, charset)

    fromReader(reader, options)
  }

  def fromReader[@specialized T](reader: Reader)(implicit deserializer: Deserializer[T]): T = {
    fromReader(reader, YAMLOptions.default)
  }

  def fromReader[@specialized T](reader: Reader, options: YAMLOptions)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new YAMLReaderInput(reader, options))
  }

  private def writeYAML[T](v: T, writer: Writer, dumperOptions: DumperOptions)(implicit serializer: Serializer[T]): Unit = {
    val yamlSerializer: YAMLSerializer = new YAMLSerializer(new Emitter(writer, dumperOptions), defaultResolver, dumperOptions, null)

    //try {
      yamlSerializer.open()
      serializer.serializeRaw(new YAMLOutput(yamlSerializer, dumperOptions), v)
      yamlSerializer.close()
    //} catch {
    //  case ex: Exception => throw new YAMLException(ex) // TODO: better exception?
    //}
  }
}