package test.fixtures

import humbug.codecs.TEnumCodec
import org.scalacheck.{ Gen, Arbitrary }
import scala.util.Try

object ContentType extends Enumeration {
  type ContentType = Value

  val ARTICLE = Value(0)
  val LIVEBLOG = Value(1)
  val GALLERY = Value(2)
  val INTERACTIVE = Value(3)
  val PICTURE = Value(4)
  val VIDEO = Value(5)
  val CROSSWORD = Value(6)
  val AUDIO = Value(7)

  implicit val codec: TEnumCodec[ContentType] = new TEnumCodec[ContentType] {
    def encode = _.id
    def decode = i ⇒ Try {
      ContentType(i)
    }.toOption
  }

  import Arbitrary._
  implicit val arb: Arbitrary[ContentType] = Arbitrary(Gen.oneOf(
    ARTICLE,
    LIVEBLOG,
    GALLERY,
    INTERACTIVE,
    PICTURE,
    VIDEO,
    CROSSWORD,
    AUDIO
  ))

}
