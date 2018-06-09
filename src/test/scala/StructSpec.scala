package test

import humbug._
import humbug.codecs._
import org.scalacheck.{ Gen, Properties }
import org.scalacheck.Prop.{ forAll, BooleanOperators }

import test.fixtures._

object StructSpec extends Properties("Structs") {
  val codec = implicitly[TStructCodec[Rights]]

  property("codec") = forAll { (r: Rights) ⇒
    codec.decode(codec.encode(r)) == Some(r)
  }
}
