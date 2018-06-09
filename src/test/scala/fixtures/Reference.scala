package test.fixtures

import humbug.TStruct
import humbug.codecs._

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

case class Reference(
    id:     String,
    `type`: String
)

object Reference {
  import Arbitrary._

  implicit val arbi: Arbitrary[Reference] = Arbitrary {
    for {
      id ← arbitrary[String]
      `type` ← arbitrary[String]
    } yield Reference(id, `type`)
  }

  implicit val codec = new TStructCodec[Reference] {
    final val fieldIds = List(1, 2)

    final val defaults = Map.empty

    def encode = r ⇒ Map(
      1.toShort -> Dyn(r.id, TyString),
      2.toShort -> Dyn(r.`type`, TyString)
    )

    def decode = m ⇒ for {
      id ← m.get(1).orElse(defaults.get(1)).flatMap(Dynamic.cast(_, TyString))
      `type` ← m.get(2).orElse(defaults.get(2)).flatMap(Dynamic.cast(_, TyString))
    } yield Reference(id, `type`)

  }
}