package test.fixtures

import humbug._
import humbug.codecs._

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

class DateTime(val value: Long) extends AnyVal with TTypeDef

object DateTime {
  import Arbitrary._

  implicit val arb: Arbitrary[DateTime] = Arbitrary {
    for { l ← arbitrary[Long] } yield new DateTime(l)
  }

  implicit val codec = new TTypeDefCodec[DateTime] {
    type Rep = Long
    val typeRep = TyI64
    def encode = _.value
    def decode = new DateTime(_)
  }
}