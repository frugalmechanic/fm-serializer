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

object YAMLOptions {
  val default: YAMLOptions = YAMLOptions()
  val internStrings: YAMLOptions = YAMLOptions(internStrings = true)

  // // Serializer(Emitable emitter, Resolver resolver, DumperOptions opts, Tag rootTag)
}

/**
 * @param internStrings Call String.intern() on any strings read
 */
final case class YAMLOptions(
  internStrings: Boolean = false
  // TODO: Add the YAMLOutput options into here and maybe split between serializer/deserialier options
) {
  
}