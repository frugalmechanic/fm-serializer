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
package fm.serializer

import fm.common.Logging
import fm.common.{IP, ImmutableArray, ImmutableDate, UUID}
import fm.serializer.validation.{Validation, ValidationError, ValidationOptions, ValidationResult}
import java.io.File
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.{LocalDate, LocalDateTime}
import java.util.{Calendar, Date}
import org.bson.types.ObjectId
import org.scalatest.{AppendedClues, FunSuite, Matchers}
import scala.collection.JavaConverters._

object TestSerializer {
  private val RepeatFactor: Int = 1024
  private val LongRepeatFactor: Int = 8190

  case class Foo (
    string: String = "Hello World!",
    int: Int = 1234,
    long: Long = 12345678912345L,
    float: Float = 3.14159f,
    double: Double = 3.14159d,
    bool: Boolean = true,
    date: Date = new Date(),
    bigInteger: JavaBigInteger = new JavaBigInteger("123456789012345678901234567890"),
    bigDecimal: JavaBigDecimal = new JavaBigDecimal("12345678901234.5678901234567890"),
    intList: Seq[Int] = List(1,2,3,4,5,6,7,8,9,10),
    stringList: Vector[String] = Vector("one", "two", "three", "four", ""),
    emptyList: Seq[String] = Nil,
    stringOpt: Option[String] = Some("Hello"),
    stringOptNone: Option[String] = None,
    listOpt: Option[List[Int]] = Some(List(1,2,3)),
    listOptNone: Option[List[String]] = None,
    tuple: (Int, String, Double) = (111, "222", 333.333),
    map: Map[String,Int] = Map("foo" -> 1, "bar" -> 2),
    bar: Bar = Bar(),
    barOpt: Option[Bar] = Some(Bar(next = Some(Bar("next", 321, Some(Bar()))))),
    barList: List[Bar] = List(Bar("one", 1), Bar("two", 2, Some(Bar("nested", 999, Some(Bar("nest2", 111))))), Bar("three", 3)),
    foo: Option[Foo] = Some(Foo(foo = None))
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
  )

  private def IntLengths: Vector[Int] = Vector(1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000)
  private def LongLengths: Vector[Long] = Vector(1L,10L,100L,1000L,10000L,100000L,1000000L,10000000L,100000000L,1000000000L,10000000000L,100000000000L,1000000000000L,10000000000000L,100000000000000L,1000000000000000L,10000000000000000L,100000000000000000L,1000000000000000000L)

  case class Bar(
    string: String = "bar",
    int: Int = 123,
    next: Option[Bar] = None,
    file: File = new File("/foo/bar/file.ext"),
    //
    // Using this class for overflow since it would put Foo over the 22 case class param limit:
    //
    emptyString: String = "",
    nullString: String = null,
    specialChars: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!",
    minInt: Int = Int.MinValue,
    maxInt: Int = Int.MaxValue,
    minLong: Long = Long.MinValue,
    maxLong: Long = Long.MaxValue,
    intLengthChecks: Seq[Int] = IntLengths ++ Vector(Int.MinValue, Int.MaxValue),
    intLengthChecksNeg: Seq[Int] = IntLengths.map{ _ * -1 },
    longLengthChecks: Seq[Long] = LongLengths ++ Vector(Long.MinValue, Long.MaxValue),
    longLengthChecksNeg: Seq[Long] = LongLengths.map{ _ * -1 },
    javaInt: Integer = Integer.valueOf(987654),
    javaIntNull: Integer = null,
    // This is 120 chars long.  We should end up prefixing with a 2-byte varint that is padded since the max this string could take up is 120 * 3 (up to 3 bytes per char)
    stringPrefixCheck: String = "012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789",
    longString: String = "abcdefghijklmnopqrstuvwxyz"*LongRepeatFactor, // This should blow past the size of any output buffer to trigger slow string write paths
    multiByteLongString: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!"*RepeatFactor,
    anothermultiByteLongString: String = "\u0024\u00A2\u20AC"*LongRepeatFactor,
    baz: Baz = Baz()
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
    // Don't add more to this class (it already has 22 items) until we stop supporting Scala 2.10.x
  )

  // Additional overflow since Foo & Bar have 22 items
  case class Baz(
    // Should deserialize as a Vector
    iterable: Iterable[String] = List("one","two","three"),
    children: scala.collection.IndexedSeq[Baz] = Vector(Baz(children = Vector.empty), Baz(children = Vector.empty)),
    indexedSeq: IndexedSeq[String] = Vector("foo0","bar0","baz0"),
    scalaIndexedSeq: scala.IndexedSeq[String] = Vector("foo1","bar1","baz1"),
    collectionIndexedSeq: scala.collection.IndexedSeq[String] = Vector("foo2","bar2","baz2"),
    immutableIndexedSeq: scala.collection.immutable.IndexedSeq[String] = Vector("foo3","bar3","baz3"),
    mutableIndexedSeq: scala.collection.mutable.IndexedSeq[String] = scala.collection.mutable.IndexedSeq("foo4","bar4","baz4"),
    emptyIndexedSeq: IndexedSeq[String] = IndexedSeq.empty,
    emptyVector: Vector[String] = Vector.empty,
    char: Char = 'A',
    calendar: Calendar = Calendar.getInstance,
    calendarNull: Calendar = null,
    dateNull: Date = null,
    localDate: LocalDate = LocalDate.now,
    localDateTime: LocalDateTime = LocalDateTime.now,
    localDateNull: LocalDate = null,
    localDateTimeNull: LocalDateTime = null,
    bsonTypes: BsonTypes = BsonTypes(),
    fmCommonTypes: FMCommonTypes = FMCommonTypes(),
    supplementaryCharacters: SupplementaryCharacters = SupplementaryCharacters(),
    bigIntegerAndDecimalTypes: BigIntegerAndDecimalTypes = BigIntegerAndDecimalTypes()
  )

  // Supplementary Characters are represented in Java as 2 characters but need
  // to be converted to a single UTF-8 character (1-4 bytes) when we serialize
  // http://www.oracle.com/us/technologies/java/supplementary-142654.html
  case class SupplementaryCharacters(
    single: String = "\uD83D\uDCA5", // "💥"
    mixed: String = "foo\uD83D\uDCA5bar", // "foo💥bar"
    repeatedMixed: String = "foo\uD83D\uDCA5bar" * 10,
    emojis: String = new String(Array(0x1F600, 0x1F63A, 0x1F9D7, 0x1F1FA, 0x1F1F8).flatMap{ Character.toChars(_) }), // "😀😺🧗🇺🇸"
    repeatedEmojis: String = new String(Array(0x1F600, 0x1F63A, 0x1F9D7, 0x1F1FA, 0x1F1F8).flatMap{ Character.toChars(_) }) * RepeatFactor,
    moreMixed: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!",
    repeatedMoreMixed: String = "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!" * RepeatFactor,
    moreExamples1: String = "\u0000",
    moreExamples2: String = "💥",
    moreExamples3: String = "\uD83D\uDCA5",
    moreExamples4: String = "\uD83D\uDCA5"*99,
    moreExamples5: String = "\u0000💥\u0000"*99,
    moreExamples6: String = "ᒪ𝜎ⲅ𝚎ｍ 𝒊ϱ𝔰𝒖ｍ 𝔡ﮬl𝜊𝘳 𝘴і𝙩 𝕒ｍ𝖊𝚝, 𝘤ﮨℼ𐑈е𝞃𝓮𝕥𝛖𝘳 𝒔а𝗱ꙇ𝒑𝗌𝒸і𝑛ᶃ еlι𝘵ⲅ, 𝖘𝒆Ꮷ 𝕕ı⍺ｍ ℼ۵𝝕𝑢ｍɣ 𝖊ℹ𝓻ｍ𐐬𝚍 𝖙ｅｍ𝓹ﮨ𝕣 𝕚ռ𝕧𝜾𝖉ｕп𝐭 𝔲т l𝓪𝕓օᴦ𝚎 е𝑡 𝓭ﮩlﮦⲅ𝓮 ｍɑ𝐠𝞏𝒂 𝖆l𝑖գ𝙪𝕪𝐚ｍ 𝑒𝔯ɑ𝛕, 𝓼𝖊𝙙 ꓒ𝗶𝐚ｍ 𝖛𝞸l𝚞𝛠τ𝙪𝝰. 𝗔𝛕 ⋁𝐞𝙧𝑜 𝚎ⲟ𝗌 𝐞ᴛ 𝜶𝖈ᴄ𝘶ѕ𝚊ｍ 𝘦𝗍 𝗃ｕꜱт𝗼 𝔡ц𝔬 ꓒ𝔬l𝞼𝓻ⅇ𝙨 𝗲𝔱 е𝒂 𝓻𝓮𝔟𝕦ｍ. 𝕾𝙩𝓮𝘁 𝙘l𝓲𝘵𝓪 𝝹𝛂𝘀ⅾ 𝖌𝝊Ƅ℮ᴦ𝙜г𝒆𝕟, 𝜛𝕠 𝕤𝖊𝐚 𝕥𝙖𝖐ｉｍ𝙖𝘁𝐚 𝐬𝙖𝚗𝓬𝗍𝙪ѕ 𝐞𝘀𝜏 𐑃ᴑᴦ𝔢ｍ ͺ𝛠𝐬𝐮ｍ 𝘥၀lﮩ𝓻 ꜱ𝑖𝒕 𝖺ｍ𝕖τ. 𝕷𝞸𝓻𝖊ｍ 𝚤𝓅𝑠𝓾ｍ 𝐝੦l૦ᴦ 𝖘ͺ𝖙 𝔞ｍ𝑒𝞃, 𝒄ہｎ𝑠℮𝓽𝘦𝙩𝑢𝑟 𝑠𝗮𝚍𝚤𝝆𝒔𝓬і𝚗𝐠 ｅl𝚤𝑡𝖗, ꜱ𝑒𝔡 Ꮷ𝗶𝘢ｍ 𝚗ﮬ𝛑𝗎ｍ𝘺 𝑒ι𝘳ｍﻪᑯ т𝔢ｍ𝝔𝓸г ɩℼ⋁ℹ𝗱𝖚𝜋𝔱 𝜐𝘁 l𝞪𝘣၀𝕣𝐞 𝓮𝘁 ｄ𝞼lﮬ𝓇𝖾 ｍ𝜶𝔤𝞏𝝰 ⍺lⅈԛ𝙪ү𝑎ｍ 𝗲гɑ𝞃, 𝘀𝙚ᑯ 𝒅⍳𝘢ｍ ט𝒐l𝒖𝛒𝗍𝗎𝜶. 𝔄𝖙 𝗏е𝚛𝝄 ℯﮫƽ 𝖾ｔ 𝝰𝗰ϲ𝔲ѕａｍ ℯｔ ϳ𝝊𐑈𝗍𝛐 ｄ𝗎ه 𝖉οlσг𝙚𝙨 𝚎𝘵 ℮𝞪 𝑟𝚎𝖇𝑢ｍ. 𝕾𝒕𝙚𝖙 𝗰l𝔦𝓉𝖺 𝜘𝞪ƽ𝕕 𝗴𝜐Ꮟℯｒƍ𝐫еℼ, 𝕟σ ѕｅ𝛂 𝕥𝒂𝔨𝚤ｍ𝒶𝐭𝒶 𝘴⍺𝔫ϲｔ𝗎𝑠 𝗲𝚜𝞽 ⳑه𝔯𝔢ｍ ⍳𝒑ꜱ𝒖ｍ 𝓭ﮨlھ𝗿 𝒔ꙇ𝚝 𝚊ｍ𝗲τ.",
    moreExamples7: String = "ᒪ\uD835\uDF0Eⲅ\uD835\uDE8Eｍ \uD835\uDC8Aϱ\uD835\uDD30\uD835\uDC96ｍ \uD835\uDD21ﮬl\uD835\uDF0A\uD835\uDE33 \uD835\uDE34і\uD835\uDE69 \uD835\uDD52ｍ\uD835\uDD8A\uD835\uDE9D, \uD835\uDE24ﮨℼ\uD801\uDC48е\uD835\uDF83\uD835\uDCEE\uD835\uDD65\uD835\uDED6\uD835\uDE33 \uD835\uDC94а\uD835\uDDF1ꙇ\uD835\uDC91\uD835\uDDCC\uD835\uDCB8і\uD835\uDC5Bᶃ еlι\uD835\uDE35ⲅ, \uD835\uDD98\uD835\uDC86Ꮷ \uD835\uDD55ı⍺ｍ ℼ۵\uD835\uDF55\uD835\uDC62ｍɣ \uD835\uDD8Aℹ\uD835\uDCFBｍ\uD801\uDC2C\uD835\uDE8D \uD835\uDD99ｅｍ\uD835\uDCF9ﮨ\uD835\uDD63 \uD835\uDD5Aռ\uD835\uDD67\uD835\uDF3E\uD835\uDD89ｕп\uD835\uDC2D \uD835\uDD32т l\uD835\uDCEA\uD835\uDD53օᴦ\uD835\uDE8E е\uD835\uDC61 \uD835\uDCEDﮩlﮦⲅ\uD835\uDCEE ｍɑ\uD835\uDC20\uD835\uDF8F\uD835\uDC82 \uD835\uDD86l\uD835\uDC56գ\uD835\uDE6A\uD835\uDD6A\uD835\uDC1Aｍ \uD835\uDC52\uD835\uDD2Fɑ\uD835\uDED5, \uD835\uDCFC\uD835\uDD8A\uD835\uDE59 ꓒ\uD835\uDDF6\uD835\uDC1Aｍ \uD835\uDD9B\uD835\uDFB8l\uD835\uDE9E\uD835\uDEE0τ\uD835\uDE6A\uD835\uDF70. \uD835\uDDD4\uD835\uDED5 ⋁\uD835\uDC1E\uD835\uDE67\uD835\uDC5C \uD835\uDE8Eⲟ\uD835\uDDCC \uD835\uDC1Eᴛ \uD835\uDF36\uD835\uDD88ᴄ\uD835\uDE36ѕ\uD835\uDE8Aｍ \uD835\uDE26\uD835\uDDCD \uD835\uDDC3ｕꜱт\uD835\uDDFC \uD835\uDD21ц\uD835\uDD2C ꓒ\uD835\uDD2Cl\uD835\uDFBC\uD835\uDCFBⅇ\uD835\uDE68 \uD835\uDDF2\uD835\uDD31 е\uD835\uDC82 \uD835\uDCFB\uD835\uDCEE\uD835\uDD1F\uD835\uDD66ｍ. \uD835\uDD7E\uD835\uDE69\uD835\uDCEE\uD835\uDE01 \uD835\uDE58l\uD835\uDCF2\uD835\uDE35\uD835\uDCEA \uD835\uDF79\uD835\uDEC2\uD835\uDE00ⅾ \uD835\uDD8C\uD835\uDF4AƄ℮ᴦ\uD835\uDE5Cг\uD835\uDC86\uD835\uDD5F, \uD835\uDF1B\uD835\uDD60 \uD835\uDD64\uD835\uDD8A\uD835\uDC1A \uD835\uDD65\uD835\uDE56\uD835\uDD90ｉｍ\uD835\uDE56\uD835\uDE01\uD835\uDC1A \uD835\uDC2C\uD835\uDE56\uD835\uDE97\uD835\uDCEC\uD835\uDDCD\uD835\uDE6Aѕ \uD835\uDC1E\uD835\uDE00\uD835\uDF0F \uD801\uDC43ᴑᴦ\uD835\uDD22ｍ ͺ\uD835\uDEE0\uD835\uDC2C\uD835\uDC2Eｍ \uD835\uDE25၀lﮩ\uD835\uDCFB ꜱ\uD835\uDC56\uD835\uDC95 \uD835\uDDBAｍ\uD835\uDD56τ. \uD835\uDD77\uD835\uDFB8\uD835\uDCFB\uD835\uDD8Aｍ \uD835\uDEA4\uD835\uDCC5\uD835\uDC60\uD835\uDCFEｍ \uD835\uDC1D੦l૦ᴦ \uD835\uDD98ͺ\uD835\uDD99 \uD835\uDD1Eｍ\uD835\uDC52\uD835\uDF83, \uD835\uDC84ہｎ\uD835\uDC60℮\uD835\uDCFD\uD835\uDE26\uD835\uDE69\uD835\uDC62\uD835\uDC5F \uD835\uDC60\uD835\uDDEE\uD835\uDE8D\uD835\uDEA4\uD835\uDF46\uD835\uDC94\uD835\uDCECі\uD835\uDE97\uD835\uDC20 ｅl\uD835\uDEA4\uD835\uDC61\uD835\uDD97, ꜱ\uD835\uDC52\uD835\uDD21 Ꮷ\uD835\uDDF6\uD835\uDE22ｍ \uD835\uDE97ﮬ\uD835\uDED1\uD835\uDDCEｍ\uD835\uDE3A \uD835\uDC52ι\uD835\uDE33ｍﻪᑯ т\uD835\uDD22ｍ\uD835\uDF54\uD835\uDCF8г ɩℼ⋁ℹ\uD835\uDDF1\uD835\uDD9A\uD835\uDF0B\uD835\uDD31 \uD835\uDF10\uD835\uDE01 l\uD835\uDFAA\uD835\uDE23၀\uD835\uDD63\uD835\uDC1E \uD835\uDCEE\uD835\uDE01 ｄ\uD835\uDFBClﮬ\uD835\uDCC7\uD835\uDDBE ｍ\uD835\uDF36\uD835\uDD24\uD835\uDF8F\uD835\uDF70 ⍺lⅈԛ\uD835\uDE6Aү\uD835\uDC4Eｍ \uD835\uDDF2гɑ\uD835\uDF83, \uD835\uDE00\uD835\uDE5Aᑯ \uD835\uDC85⍳\uD835\uDE22ｍ ט\uD835\uDC90l\uD835\uDC96\uD835\uDED2\uD835\uDDCD\uD835\uDDCE\uD835\uDF36. \uD835\uDD04\uD835\uDD99 \uD835\uDDCFе\uD835\uDE9B\uD835\uDF44 ℯﮫƽ \uD835\uDDBEｔ \uD835\uDF70\uD835\uDDF0ϲ\uD835\uDD32ѕａｍ ℯｔ ϳ\uD835\uDF4A\uD801\uDC48\uD835\uDDCD\uD835\uDED0 ｄ\uD835\uDDCEه \uD835\uDD89οlσг\uD835\uDE5A\uD835\uDE68 \uD835\uDE8E\uD835\uDE35 ℮\uD835\uDFAA \uD835\uDC5F\uD835\uDE8E\uD835\uDD87\uD835\uDC62ｍ. \uD835\uDD7E\uD835\uDC95\uD835\uDE5A\uD835\uDD99 \uD835\uDDF0l\uD835\uDD26\uD835\uDCC9\uD835\uDDBA \uD835\uDF18\uD835\uDFAAƽ\uD835\uDD55 \uD835\uDDF4\uD835\uDF10Ꮟℯｒƍ\uD835\uDC2Bеℼ, \uD835\uDD5Fσ ѕｅ\uD835\uDEC2 \uD835\uDD65\uD835\uDC82\uD835\uDD28\uD835\uDEA4ｍ\uD835\uDCB6\uD835\uDC2D\uD835\uDCB6 \uD835\uDE34⍺\uD835\uDD2Bϲｔ\uD835\uDDCE\uD835\uDC60 \uD835\uDDF2\uD835\uDE9C\uD835\uDFBD ⳑه\uD835\uDD2F\uD835\uDD22ｍ ⍳\uD835\uDC91ꜱ\uD835\uDC96ｍ \uD835\uDCEDﮨlھ\uD835\uDDFF \uD835\uDC94ꙇ\uD835\uDE9D \uD835\uDE8Aｍ\uD835\uDDF2τ."
  )

  case class BsonTypes(
    objectId: ObjectId = new ObjectId(),
    objectIdNull: ObjectId = null,
    someObjectId: Option[ObjectId] = Some(new ObjectId()),
    noneObjectId: Option[ObjectId] = None,
    objectIds: Vector[ObjectId] = Vector(new ObjectId(), new ObjectId(), new ObjectId(), new ObjectId())
    // Nulls within collections aren't fully supported yet
    //objectIdsWithNulls: Vector[ObjectId] = Vector(new ObjectId(), null, new ObjectId(), null)
  )

  case class FMCommonTypes(
    immutableDate: ImmutableDate = ImmutableDate(),
    immutableDateNull: ImmutableDate = null,
    someImmutableDate: Option[ImmutableDate] = Some(ImmutableDate()),
    noneImmutableDate: Option[ImmutableDate] = None,
    immutableDates: Vector[ImmutableDate] = Vector(ImmutableDate(1), ImmutableDate(2), ImmutableDate()),
    // Nulls within collections aren't fully supported yet
    //immutableDatesNull: Vector[ImmutableDate] = Vector(ImmutableDate(1), ImmutableDate(2), null),
    ipMin: IP = IP.empty,
    ipMid: IP = IP("123.123.123.123"),
    ipMax: IP = IP("255.255.255.255"),
    ipNone: Option[IP] = None,
    ipSome: Option[IP] = Some(IP("192.168.0.1")),
    bytes: ImmutableArray[Byte] = ImmutableArray.wrap("Hello World!".getBytes(UTF_8)),
    moreBytes: ImmutableArray[Byte] = ImmutableArray.wrap(("Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!"*RepeatFactor).getBytes(UTF_8)),
    bytesNull: ImmutableArray[Byte] = null,
    immutableArrayInts: ImmutableArray[Int] = ImmutableArray(1,2,3,4,Int.MinValue,Int.MaxValue),
    immutableArrayLongs: ImmutableArray[Long] = ImmutableArray(1,2,3,4,Int.MinValue,Int.MaxValue,Long.MinValue,Long.MaxValue),
    immutableArrayStrings: ImmutableArray[String] = ImmutableArray("one","two","three","four"),
    emptyImmutableArrayInt: ImmutableArray[Int] = ImmutableArray.empty,
    emptyImmutableArrayLong: ImmutableArray[Long] = ImmutableArray.empty,
    emptyImmutableArrayString: ImmutableArray[String] = ImmutableArray.empty,
    uuid: UUID = UUID(),
    uuidNull: UUID = null,
    uuidSome: Option[UUID] = Some(UUID()),
    uuidNone: Option[UUID] = None
  )

  case class BigIntegerAndDecimalTypes(
    javaBigInteger: JavaBigInteger = new JavaBigInteger("123456789012345678901234567890"),
    javaBigIntegerNull: JavaBigInteger = null,
    javaBigDecimal: JavaBigDecimal = new JavaBigDecimal("12345678901234.5678901234567890"),
    javaBigDecimalNull: JavaBigDecimal = null,
    scalaBigInt: BigInt = BigInt("123456789012345678901234567890"),
    scalaBigIntNull: BigInt = null,
    scalaBigDecimal: BigDecimal = BigDecimal("12345678901234.5678901234567890"),
    scalaBigDecimalNull: BigDecimal = null
  )

  case class MostlyEmptyFoo(@Field(19) bar: Bar)

  case class ValidationCls(
    @Field(1) string: String,
    @Field(2) int: Int,
    @Field(3) option: Option[String],
    @Field(4) nested: NestedValidation,
    @Field(5) defaultValue: String = "default",
    @Field(6) defaultOptionValue: Option[String] = None,
    @Field(7) anotherDefaultValue: Option[String] = Some("default_value")
  )

  case class NestedValidation(foo: String, bar: Int)
  case class ValidationMissingNestedCls(string: String, int: Int, option: Option[String], nested: NestedValidationMissingField)
  case class NestedValidationMissingField(foo: String)

  // Wrong type for "int" field
  case class ValidationWrongType(string: String, int: String)

  // Missing "option" and "nested"
  case class ValidationMissingOptionAndNestedFields(string: String, int: Int)

  // Missing "nested"
  case class ValidationMissingNestedField(string: String, int: Int, option: Option[String])

  // Uses defaults for defaultValue and defaultOptionValue
  case class ValidationUsesDefaults(string: String, int: Int, option: Option[String], nested: NestedValidation)

  // Extra "qweqwe" field
  case class ValidationExtraField(
    string: String,
    int: Int,
    option: Option[String],
    nested: NestedValidation,
    defaultValue: String,
    defaultOptionValue: Option[String],
    anotherDefaultValue: Option[String],
    qweqwe: String
  )
}

trait TestSerializer[BYTES] extends FunSuite with Matchers with AppendedClues {
  import TestSerializer._

  // Does the serialization method support serializing raw collections
  def supportsRawCollections: Boolean = true

  // Does the serialization method support serializing primitives
  def supportsPrimitives: Boolean = true

  def serialize[T](v: T)(implicit ser: Serializer[T]): BYTES
  def deserialize[T](bytes: BYTES)(implicit deser: Deserializer[T]): T

  // This should return the ProtobufInput/JsonInput/etc and is for testing the ValidatingInput
  def makeInput(bytes: BYTES): Input

  //===============================================================================================
  // Primitive Testing
  //===============================================================================================
  if (supportsPrimitives) {
    test("Int Primitives") {
      checkPrimitive(123)
      checkPrimitive(Int.MinValue)
      checkPrimitive(Int.MaxValue)
    }

    test("Long Primitives") {
      checkPrimitive(1234567890123L)
      checkPrimitive(Long.MinValue)
      checkPrimitive(Long.MaxValue)
    }

    test("String Primitives") {
      checkPrimitive("foo")
    }

    test("ImmutableArray[Byte] Primitives") {
      checkPrimitive(ImmutableArray.wrap("Hello World!".getBytes(UTF_8)))
      checkPrimitive(ImmutableArray.wrap(("Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!"*RepeatFactor).getBytes(UTF_8)))
    }
  }

  private def checkPrimitive[@specialized T](value: T)(implicit ser: Serializer[T], deser: Deserializer[T]): Unit = {
    deserialize[T](serialize(value)) shouldBe value
  }

  //===============================================================================================
  // Validation Testing
  //===============================================================================================
  test("Validation - Round trip - @Field annotation with default values") {
    val value: ValidationCls = ValidationCls("string", 123, Some("option"), NestedValidation("foo", 321))
    deserialize[ValidationCls](serialize(value)) shouldBe value
  }

  test("Validation - Success - Default Values and options") {
    checkValidation(
      obj = ValidationCls("string", 123, Some("option"), NestedValidation("foo", 321)),
      expected = ValidationResult.Success
    )
  }

  test("Validation - Success - Default Values and report unset fields") {
    checkValidation(
      obj = ValidationCls("string", 123, Some("option"), NestedValidation("foo", 321)),
      expected = ValidationResult.Success,
      options = ValidationOptions(
        ignoreUnknownFields = false,
        ignoreUnsetFields = false,
        reportUnsetFieldsWithDefaultValues = false,
        reportUnsetOptionFields = true
      )
    )
  }

  test("Validation - Success - Explicit values, report unset fields") {
    checkValidation(
      obj = ValidationCls("string", 123, Some("option"), NestedValidation("foo", 321), "asd", Some("qwe")),
      expected = ValidationResult.Success,
      options = ValidationOptions(
        ignoreUnknownFields = false,
        ignoreUnsetFields = false,
        reportUnsetFieldsWithDefaultValues = true,
        reportUnsetOptionFields = true
      )
    )
  }

  test("Validation - Wrong Type") {
    checkValidation(
      obj = ValidationWrongType("string", "not int"),
      expected = ValidationResult.Failure(Vector(ValidationError.PrimitiveError("", 2, "int")(new IllegalArgumentException(""))))
    )
  }

  test("Validation - Missing option and nested Fields") {
    checkValidation(
      obj = ValidationMissingOptionAndNestedFields("string", 123),
      expected = ValidationResult.Failure(Vector(ValidationError.MissingField("", 4, "nested")))
    )
  }

  test("Validation - Missing option and nested Fields - report on unset option fields") {
    checkValidation(
      obj = ValidationMissingOptionAndNestedFields("string", 123),
      expected = ValidationResult.Failure(Vector(ValidationError.MissingField("", 3, "option"), ValidationError.MissingField("", 4, "nested"))),
      options = ValidationOptions(
        ignoreUnknownFields = false,
        ignoreUnsetFields = false,
        reportUnsetFieldsWithDefaultValues = false,
        reportUnsetOptionFields = true
      )
    )
  }

  test("Validation - Missing nested Field") {
    checkValidation(
      obj = ValidationMissingNestedField("string", 123, Some("option")),
      expected = ValidationResult.Failure(Vector(ValidationError.MissingField("", 4, "nested")))
    )
  }

  test("Validation - Nested Missing Field - Default Options") {
    checkValidation(
      obj = ValidationMissingNestedCls("string", 123, Some("option"), NestedValidationMissingField("foo")),
      expected = ValidationResult.Failure(Vector(ValidationError.MissingField("nested", 2, "bar")))
    )
  }

  test("Validation - Report on unset fields with default options") {
    checkValidation(
      obj = ValidationUsesDefaults("string", 123, Some("option"), NestedValidation("foo", 321)),
      expected = ValidationResult.Failure(Vector(ValidationError.MissingField("", 5, "defaultValue"))),
      options = ValidationOptions(
        ignoreUnknownFields = false,
        ignoreUnsetFields = false,
        reportUnsetFieldsWithDefaultValues = true,
        reportUnsetOptionFields = false
      )
    )
  }

  test("Validation - Report on unset option fields") {
    checkValidation(
      obj = ValidationUsesDefaults("string", 123, Some("option"), NestedValidation("foo", 321)),
      expected = ValidationResult.Failure(Vector(ValidationError.MissingField("", 5, "defaultValue"), ValidationError.MissingField("", 6, "defaultOptionValue"), ValidationError.MissingField("", 7, "anotherDefaultValue"))),
      options = ValidationOptions(
        ignoreUnknownFields = false,
        ignoreUnsetFields = false,
        reportUnsetFieldsWithDefaultValues = true,
        reportUnsetOptionFields = true
      )
    )
  }

  test("Validation - Extra Field") {
    val obj: ValidationExtraField = ValidationExtraField("string", 123, Some("option"), NestedValidation("foo", 321), "default", None, Some("bar"), "qwe")
    val bytes: BYTES = serialize(obj)
    val res: ValidationResult = Validation.validate[ValidationCls](makeInput(bytes))

    res.isFailure shouldBe true
    res.errors.length shouldBe 1

    val error: ValidationError = res.errors.head

    error.isInstanceOf[ValidationError.UnknownField] shouldBe true

    // One or both of these should be set depending on if the serialization uses field numbers or field names
    require(error.fieldNumber === 8 || error.fieldName === "qweqwe")
  }

  private def checkValidation[T](obj: T, expected: ValidationResult, options: ValidationOptions = ValidationOptions.default)(implicit ser: Serializer[T]): Unit = {
    val bytes: BYTES = serialize(obj)
    val res: ValidationResult = Validation.validate[ValidationCls](makeInput(bytes), options)
    res shouldBe expected
  } withClue s"obj: $obj, expected: $expected"

  //===============================================================================================
  // Simple Object Testing
  //===============================================================================================
  
  case class PersonNoAge(name: String)
  case class Person(name: String, age: Int = 18, blah: String = "blah")
  case class LinkedPerson(person: Person, next: LinkedPerson)

  test("Person") {
    val p: Person = Person("Bob", 123)
    val bytes: BYTES = serialize(p)
    val p2: Person = deserialize[Person](bytes)
    
    p2.name should equal (p.name)
    p2.age should equal (p.age)
    
    p2 should equal (p)
  }
  
  test("Person - Default Values for missing fields") {
    val p: PersonNoAge = PersonNoAge("Bob")
    val bytes: BYTES = serialize(p)
    val p2: Person = deserialize[Person](bytes)
    
    p2.name should equal (p.name)
    p2.age should equal (18)
    p2.blah should equal ("blah")
  }
  
  test("Linked Person - 1") {
    val p: LinkedPerson = LinkedPerson(Person("one", 1, "1"), null)
    val bytes: BYTES = serialize(p)
    val p2: LinkedPerson = deserialize[LinkedPerson](bytes)
    
    p2 should equal (p)
  }
  
  test("Linked Person - 2") {    
    val p: LinkedPerson = LinkedPerson(Person("one", 1, "1"), LinkedPerson(Person("two", 2, "2"), null))
    val bytes: BYTES = serialize(p)    
    val p2: LinkedPerson = deserialize[LinkedPerson](bytes)
    
    p2 should equal (p)
  }
  
  test("Linked Person - 3") {    
    val p: LinkedPerson = LinkedPerson(Person("one", 1, "1"), LinkedPerson(Person("two", 2, "2"), LinkedPerson(Person("three", 3, "3"), null)))
    val bytes: BYTES = serialize(p)
    val p2: LinkedPerson = deserialize[LinkedPerson](bytes)
    
    p2 should equal (p)
  }
  
  //===============================================================================================
  // Option Testing
  //===============================================================================================
  case class OptionalFoo(string: Option[String], bool: Option[Boolean], char: Option[Char], int: Option[Int], long: Option[Long])
  
  test("Option Handling - None") {
    val foo: OptionalFoo = OptionalFoo(None, None, None, None, None)
    val bytes: BYTES = serialize(foo)
    val foo2: OptionalFoo = deserialize[OptionalFoo](bytes)
    
    foo should equal (foo2)
  }
  
  test("Option Handling - Some") {
    val foo: OptionalFoo = OptionalFoo(Some("Hello World!"), Some(true), Some('A'), Some(123), Some(456L))
    val bytes: BYTES = serialize(foo)
    val foo2: OptionalFoo = deserialize[OptionalFoo](bytes)
    
    foo should equal (foo2)
  }

  test("Option Handling - Some - Cached Values") {
    import fm.common.Implicits._

    val foo: OptionalFoo = OptionalFoo(Some("Hello World!"), Some.cached(true), Some.cached('A'), Some.cached(1), Some.cached(2L))
    val bytes: BYTES = serialize(foo)
    val foo2: OptionalFoo = deserialize[OptionalFoo](bytes)

    foo should equal (foo2)
    foo.bool should be theSameInstanceAs foo2.bool
    foo.char should be theSameInstanceAs foo2.char
    foo.int should be theSameInstanceAs foo2.int
    foo.long should be theSameInstanceAs foo2.long
  }
  
  case class OptionalFooWithDefaults(string: Option[String] = Some("default"), bool: Option[Boolean] = Some(false), int: Option[Int] = Some(123456789), long: Option[Long] = Some(987654321L))
  
  test("Option Handling with Defaults - None") {
    val foo: OptionalFooWithDefaults = OptionalFooWithDefaults(None, None)
    val bytes: BYTES = serialize(foo)
    val foo2: OptionalFooWithDefaults = deserialize[OptionalFooWithDefaults](bytes)
    
    if (!ignoreNullRetainTest) foo should equal (foo2)
  }
  
  test("Option Handling with Defaults - Some") {
    val foo: OptionalFooWithDefaults = OptionalFooWithDefaults(Some("Hello World!"), Some(true), Some(123), Some(456L))
    val bytes: BYTES = serialize(foo)
    val foo2: OptionalFooWithDefaults = deserialize[OptionalFooWithDefaults](bytes)
    
    foo should equal (foo2)
  }
  
  //===============================================================================================
  // Complex Object Testing
  //===============================================================================================
  
  test("Foo") {
    val foo: Foo = Foo()
    val bytes: BYTES = serialize(foo)
    logger.error(s"Foo: ${fm.serializer.yaml.YAML.toYAML(foo).slice(0,300)}")
    val foo2: Foo = deserialize[Foo](bytes)

    // TestMinimalJSON doesn't play well with this
    if (!ignoreNullRetainTest) {
      foo2.foo should equal (foo.foo)
      
      foo2 should equal (foo)
    }
    
    // Iterable doesn't have a CanBuildFrom so we default to using a Vector
    require(foo2.bar.baz.iterable.isInstanceOf[Vector[String]])
  }
  
  test("Foo - Skipping unknown fields") {
    val foo: Foo = Foo()
    val bytes: BYTES = serialize(foo)
    val emptyFoo: MostlyEmptyFoo = deserialize[MostlyEmptyFoo](bytes)
    
    emptyFoo.bar should equal (foo.bar)
  }
  
  //===============================================================================================
  // Annotated Object Testing
  //===============================================================================================
  
  case class Car (year: Int = 1234, make: String = "default make", model: String = "default model")
  
  case class CarReversed(
    @Field(3, "model") the_model: String,
    @Field(2, "make") the_make: String,
    @Field(1, "year") the_year: Int
  )
  
  case class CarWithJustYear(year: Int = 4321)
  
  test("Car -> CarReversed") {
    val car: Car = Car(2005, "Subaru", "Legacy")
    val bytes: BYTES = serialize(car)
    val car2: CarReversed = deserialize[CarReversed](bytes)
    
    car2.the_year should equal (car.year)
    car2.the_make should equal (car.make)
    car2.the_model should equal (car.model)
  }
  
  test("CarReversed -> Car") {
    val car: CarReversed = CarReversed("Legacy", "Subaru", 2005)
    val bytes: BYTES = serialize(car)
    val car2: Car = deserialize[Car](bytes)
    
    car2.year should equal (car.the_year)
    car2.make should equal (car.the_make)
    car2.model should equal (car.the_model)
  }
  
  //===============================================================================================
  // Nulls
  //===============================================================================================
  def ignoreNullRetainTest: Boolean = false
  
  test("Car with nulls should retain nulls") {
    val car: Car = Car(2005, null, null)
    val bytes: BYTES = serialize(car)
    val car2: Car = deserialize[Car](bytes)
    
    // In TestMinimalJSON we ignore this check since we don't output nulls
    // and end up using the default values
    if (!ignoreNullRetainTest) car should equal (car2)
  }
  
  test("Default values should be used for missing fields") {
    val car: CarWithJustYear = CarWithJustYear(2005)
    val bytes: BYTES = serialize(car)
    val car2: Car = deserialize[Car](bytes)
    
    car2.year should equal (2005)
    car2.make should equal ("default make")
    car2.model should equal ("default model")
  }
  
  //===============================================================================================
  // Case-like Classes
  //===============================================================================================
  
  class CaseLike(val name: String, var age: Int)
  
  test("Case-Like Class") {
    val caseLike = new CaseLike("foo", 321)
    
    val bytes: BYTES = serialize(caseLike)
    val caseLike2: CaseLike = deserialize[CaseLike](bytes)
    
    caseLike2.name should equal (caseLike.name)
    caseLike2.age should equal (caseLike.age)
  }


  test("FooJavaBeanContainer") {
    val container: FooJavaBeanContainer = new FooJavaBeanContainer()
    
    val bytes: BYTES = serialize(container)
    val container2: FooJavaBeanContainer = deserialize[FooJavaBeanContainer](bytes)
  }
      
  //===============================================================================================
  // Java Beans
  //===============================================================================================
  test("FooJavaBean") {
    val foo: FooJavaBean = new FooJavaBean()
    foo.setName("Hello World")
    foo.setNumber(123)
    foo.setBool(true)
    foo.setFooEnum(FooEnum.Bar)
    foo.setChildren(Vector({
      val child: FooJavaBean = new FooJavaBean()
      child.setName("Hello World Child")
      child
    }).asJava)
    foo.setList(Vector("aa", "bb", "cc").asJava)
    foo.getListWithoutSetter().addAll(Vector("One", "Two", "Three").asJava)
    foo.setIgnoredField1("ignored1")
    foo.setIgnoredField2("ignored2")
    foo.setIgnoredField4("ignored4")
    foo.setShadowedInterfaceMethod("not transient")
    
    val bytes: BYTES = serialize(foo)
    val foo2: FooJavaBean = deserialize[FooJavaBean](bytes)
    
    foo2.getName should equal (foo.getName)
    foo2.getNumber should equal (foo.getNumber)
    foo2.isBool should equal (foo.isBool)
    foo2.getFooEnum should equal (foo.getFooEnum)
    foo2.getChildren.get(0).getName() should equal(foo.getChildren.get(0).getName())
    foo2.getList.asScala should equal (foo.getList.asScala)
    foo2.getListWithoutSetter.asScala should equal (foo.getListWithoutSetter.asScala)
    foo2.getIgnoredField1 should equal (null)
    foo2.getIgnoredField2 should equal (null)
    
    // This will fail until proper java transient field detection is in place
    foo2.getIgnoredField4 should equal (null)

    foo2.getShadowedInterfaceMethod should equal("not transient")

  }
  
  //===============================================================================================
  // Java Beans Immutable
  //===============================================================================================
  test("FooJavaBeanImmutable") {
    val foo: FooJavaBeanImmutable = new FooJavaBeanImmutable("Hello World", 123, true, FooEnum.Bar, Vector("aa", "bb", "cc").asJava)
    
    val bytes: BYTES = serialize(foo)
    val foo2: FooJavaBeanImmutable = deserialize[FooJavaBeanImmutable](bytes)
    
    foo2.getName should equal (foo.getName)
    foo2.getNumber should equal (foo.getNumber)
    foo2.isBool should equal (foo.isBool)
    foo2.getFooEnum should equal (foo.getFooEnum)
    foo2.getList.asScala should equal (foo.getList.asScala)
  }
  
  //===============================================================================================
  // Serializing a Trait based on a fields from a Case Class
  //===============================================================================================
  
  trait AbstractPerson {
    def name: String
    def age: Int
    def linked: ConcretePerson // TODO: make this work with an AbstractPerson type
  }
  
  case class ConcretePerson (name: String, age: Int, linked: ConcretePerson) extends AbstractPerson
  class ConcretePersonAlt (val name: String, val age: Int, val linked: ConcretePerson) extends AbstractPerson
  
  test("Iface / Concrete") {
    implicit val ser: Serializer[AbstractPerson] = ObjectSerializer.forInterface[AbstractPerson, ConcretePerson]()
    implicit val deser: Deserializer[ConcretePerson] = ObjectDeserializer[ConcretePerson]()

    val person: AbstractPerson = new ConcretePersonAlt("foo", 123, new ConcretePerson("linked", 321, null))
    
    val bytes: BYTES = serialize(person)
    val concrete: ConcretePerson = deserialize[ConcretePerson](bytes)
    
    concrete.name should equal (person.name)
    concrete.age should equal (person.age)
    concrete.linked.name should equal (person.linked.name)
    concrete.linked.age should equal (person.linked.age)
    concrete.linked.linked should equal (null)

  }
  
  //===============================================================================================
  // Specifying a Serializer/Deserializer via @Field annotations
  //===============================================================================================
  
  case class NonCompressedNums(
    @Field(1, fm.serializer.Primitive.fixedInt) fixedInt: Int,
    @Field(2, fm.serializer.Primitive.fixedLong) fixedLong: Long
  )
  
  test("Specifying FixedInt/FixedLong instead of the default int/long serializer") {
    val obj = NonCompressedNums(123, 321L)
    
    val bytes: BYTES = serialize(obj)
    
//    if (bytes.isInstanceOf[Array[Byte]]) {
//      val b: Array[Byte] = bytes.asInstanceOf[Array[Byte]]
//      println("BYTES: "+b.toSeq)
//    }

    val obj2: NonCompressedNums = deserialize[NonCompressedNums](bytes)
    
    obj2.fixedInt should equal (obj.fixedInt)
    obj2.fixedLong should equal (obj.fixedLong)
  }
  
  //===============================================================================================
  // Nested Collection Serializer (GitHub issue #5: https://github.com/frugalmechanic/fm-serializer/issues/5)
  //===============================================================================================
  
  case class Node(name: String, children: IndexedSeq[Node] = Vector.empty)
  
  test("Nested Nodes - Single Node") {
    val node: Node = Node("one", children = Vector(Node("foo"), Node("bar")))
    
    val bytes: BYTES = serialize(node)
    val node2: Node = deserialize[Node](bytes)
    
    node should equal (node2)
  }

  test("Nested Nodes - Single Node - Multi Level Nesting") {
    val node: Node = Node("one", children = Vector(Node("foo", children = Vector(Node("asd"), Node("qwe"))), Node("bar")))

    val bytes: BYTES = serialize(node)
    val node2: Node = deserialize[Node](bytes)

    node should equal (node2)
  }

  if (supportsRawCollections) test("Nested Nodes - Multiple Nodes") {
    val n: Node = Node("one", children = Vector(Node("foo"), Node("bar")))

    val nodes: IndexedSeq[Node] = Vector(n, n, n)
    val bytes: BYTES = serialize(nodes)
    val nodes2: IndexedSeq[Node] = deserialize[IndexedSeq[Node]](bytes)

    nodes should equal (nodes2)
  }

  //===============================================================================================
  // Renamed Fields
  //===============================================================================================

  case class NonRenamed(foo: String, bar: Int)
  case class Renamed(@RenameField("foo") renamed: String, bar: Int)

  test("Renamed Fields") {
    val p: NonRenamed = NonRenamed("Hello World", 123)
    val renamed: Renamed = Renamed("Hello World", 123)

    val bytes: BYTES = serialize(p)
    val p2: Renamed = deserialize[Renamed](bytes)

    p2 should equal (renamed)
  }


  //===============================================================================================
  // New Fields
  //===============================================================================================


  case class NewFieldOriginal(foo: String)
  case class NewFieldUpdated(@Field(2) bar: Option[Int] = None, @Field(1) foo: String)

  test("Adding a new field w/default value") {
    val orig: NewFieldOriginal = NewFieldOriginal("Hello World")
    val updated: NewFieldUpdated = NewFieldUpdated(foo = "Hello World")

    val bytes: BYTES = serialize(orig)

    val p2: NewFieldUpdated = deserialize[NewFieldUpdated](bytes)

    p2 should equal (updated)
  }




  //===============================================================================================
  // AnyVal Classes
  //===============================================================================================

  test("AnyVal classes should pass through serialization to the single field they wrap") {
    val wrapped: WrappedHolder = WrappedHolder(WrappedInt(123))
    val raw: IntHolder = IntHolder(123)

    val wrappedBytes: BYTES = serialize(wrapped)

    deserialize[WrappedHolder](wrappedBytes) should equal (wrapped)
    deserialize[IntHolder](wrappedBytes) should equal (raw)

    val rawBytes: BYTES = serialize(wrapped)

    deserialize[WrappedHolder](rawBytes) should equal (wrapped)
    deserialize[IntHolder](rawBytes) should equal (raw)
  }

  //===============================================================================================
  // Overriden AnyVal Classes
  //===============================================================================================

  case class IPHolder(ip: IP)
  case class IPAsLong(ip: Long)

  test("IP Should serialize as a long using the manually defined Serializer instead of AnyVal") {
    val ip: IPHolder = IPHolder(IP("255.255.255.255"))
    val long: IPAsLong = IPAsLong(4294967295L)

    val ipBytes: BYTES = serialize(ip)

    deserialize[IPHolder](ipBytes) should equal (ip)
    deserialize[IPAsLong](ipBytes) should equal (long)

    val longBytes: BYTES = serialize(long)

    deserialize[IPHolder](longBytes) should equal (ip)
    deserialize[IPAsLong](longBytes) should equal (long)
  }
}

// Since this is an AnyVal it must be defined separately
case class WrappedInt(value: Int) extends AnyVal
case class IntHolder(count: Int)
case class WrappedHolder(count: WrappedInt)
