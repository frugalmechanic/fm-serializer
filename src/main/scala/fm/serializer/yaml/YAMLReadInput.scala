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
import fm.common.Util
import java.io.Reader
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.events.Event
import scala.collection.JavaConverters._

final class YAMLReaderInput(reader: Reader, options: YAMLOptions) extends YAMLInput(options) with Logging {
  private val yaml: Yaml = new Yaml()


  private val (time: Long, events: Iterator[Event]) = Util.time {
    yaml.parse(reader).iterator().asScala
  }

  protected var headOption: Option[Event] = {
    // Initialize headOption, can't use readNextRequired
    val streamStart: Event = events.next()
    require(streamStart.is(Event.ID.StreamStart), s"Expected StreamStart, but got: $streamStart")

    val documentStart: Event = events.next()
    require(documentStart.is(Event.ID.DocumentStart), s"Expected StreamStart, but got: $documentStart")

    Some(events.next())
  }

  var currentField: String = ""

  logger.debug(s"SnakeYAML took $time ms to parse reader.")

  protected def debug(): Unit = {
    events.foreach { e: Event => logger.error(s"Event: $e") }
  }
  protected def isEOF(event: Event): Boolean = event.is(Event.ID.DocumentEnd) || event.is(Event.ID.StreamEnd)

  private var done: Boolean = false

  protected def hasNext(): Boolean = !done && events.hasNext && !headOption.exists{ isEOF }

  protected def readNextRequired(tpe: Event.ID): Unit = {
    require(headOption.exists{ _.is(tpe)}, s"Expected a ${tpe}, but got: $headOption")
    next()
  }
  /** The snakeyaml Events contain both the Tag and the Value, so we need to re-use a single event
   *  for both readFieldName and readRaw* values.
   **/

  private var isChildren: Boolean = false
  def setIsChildren(): Unit = isChildren = true

  // DocumentEndEvent
  /** Return peek and advance to the next character */
  protected def next(): Event = {
    require(headOption.isDefined, "EOF")

    val ret: Event = headOption.get

    headOption = if (hasNext) {
      val nextEvent: Event = events.next

      if (isEOF(nextEvent)) {
        done = true
        None
      } else {
        Some(nextEvent)
      }
    } else None

    if (isChildren) logger.error(s"IS CHILDREN: next(): $ret and next next: $headOption")
    ret
  }
}
