package test.fixtures

import humbug.codecs.TEnumCodec
import scala.util.Try

object AtomType extends Enumeration {
  type AtomType = Value

  val QUIZ = Value(0)
  val MEDIA = Value(2)
  val EXPLAINER = Value(3)
  val CTA = Value(4)
  val INTERACTIVE = Value(5)
  val REVIEW = Value(6)
  val RECIPE = Value(7)
  val STORYQUESTIONS = Value(8)
  val QANDA = Value(9)
  val PROFILE = Value(10)
  val GUIDE = Value(11)
  val TIMELINE = Value(12)

  implicit val codec: TEnumCodec[AtomType] = new TEnumCodec[AtomType] {
    def encode = _.id
    def decode = i ⇒ Try {
      AtomType(i)
    }.toOption
  }
}
