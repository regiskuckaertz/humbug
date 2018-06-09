package test

import humbug.codecs._
import org.scalacheck.{ Gen, Properties }
import org.scalacheck.Prop.{ forAll, BooleanOperators }

import test.fixtures._

object EnumSpec extends Properties("Enums") {
  val codec = implicitly[TEnumCodec[AtomType.AtomType]]

  property("codec") = forAll(Gen.choose(0, 13)) { (i: Int) ⇒
    (i < 13) ==> (codec.decode(i).map(codec.encode) == Some(i)) || codec.decode(i).map(codec.encode) == None
  }
}
