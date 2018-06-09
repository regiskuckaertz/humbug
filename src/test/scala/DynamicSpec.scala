package test

import humbug.codecs._
import org.scalacheck.{ Gen, Properties }
import org.scalacheck.Prop.forAll

import test.fixtures._
import Dynamic._

object DynamicSpec extends Properties("Dynamic") {
  include(DynamicPrimitiveSpec)
  include(DynamicOptionSpec)
  include(DynamicListSpec)
  include(DynamicSetSpec)
  include(DynamicMapSpec)
  include(DynamicStructSpec)
}

object DynamicPrimitiveSpec extends Properties("Primitives") {
  val boolProp = forAll { (b: Boolean) ⇒ cast(Dyn(b, TyBool), TyBool) == Some(b) }
  val boolProp2 = forAll { (b: Boolean) ⇒ cast(Dyn(b, TyBool), TyString) == None }
  val byteProp = forAll { (b: Byte) ⇒ cast(Dyn(b, TyByte), TyByte) == Some(b) }
  val byteProp2 = forAll { (b: Byte) ⇒ cast(Dyn(b, TyByte), TyBool) == None }
  val doubleProp = forAll { (b: Double) ⇒ cast(Dyn(b, TyDouble), TyDouble) == Some(b) }
  val doubleProp2 = forAll { (b: Double) ⇒ cast(Dyn(b, TyDouble), TyString) == None }
  val shortProp = forAll { (b: Short) ⇒ cast(Dyn(b, TyI16), TyI16) == Some(b) }
  val shortProp2 = forAll { (b: Short) ⇒ cast(Dyn(b, TyI16), TyI32) == None }
  val intProp = forAll { (b: Int) ⇒ cast(Dyn(b, TyI32), TyI32) == Some(b) }
  val intProp2 = forAll { (b: Int) ⇒ cast(Dyn(b, TyI32), TyI64) == None }
  val longProp = forAll { (b: Long) ⇒ cast(Dyn(b, TyI64), TyI64) == Some(b) }
  val longProp2 = forAll { (b: Long) ⇒ cast(Dyn(b, TyI64), TyString) == None }
  val stringProp = forAll { (b: String) ⇒ cast(Dyn(b, TyString), TyString) == Some(b) }
  val stringProp2 = forAll { (b: String) ⇒ cast(Dyn(b, TyString), TyI64) == None }

  property("primitives") =
    boolProp && byteProp && doubleProp && shortProp && intProp && longProp && stringProp
}

object DynamicOptionSpec extends Properties("Option") {
  property("Option idempotent") = forAll { (b: Option[Long]) ⇒ cast(Dyn(b, TyOpt(TyI64)), TyOpt(TyI64)) == Some(b) }
  property("Option idempotent") = forAll { (b: Option[Long]) ⇒ cast(Dyn(b, TyOpt(TyI64)), TyOpt(TyString)) == None }
  property("Option idempotent") = forAll { (b: Option[Long]) ⇒ cast(Dyn(b, TyOpt(TyI64)), TyBool) == None }
}

object DynamicListSpec extends Properties("Dynamic lists") {
  property("List idempotent") = forAll { (b: List[String]) ⇒ cast(Dyn(b, TyList(TyString)), TyList(TyString)) == Some(b) }
  property("List idempotent") = forAll { (b: List[String]) ⇒ cast(Dyn(b, TyList(TyString)), TyList(TyByte)) == None }
  property("List idempotent") = forAll { (b: List[String]) ⇒ cast(Dyn(b, TyList(TyString)), TyString) == None }
}

object DynamicSetSpec extends Properties("Dynamic lists") {
  property("Set idempotent") = forAll { (b: Set[Double]) ⇒ cast(Dyn(b, TySet(TyDouble)), TySet(TyDouble)) == Some(b) }
  property("Set idempotent") = forAll { (b: Set[Double]) ⇒ cast(Dyn(b, TySet(TyDouble)), TySet(TyI64)) == None }
  property("Set idempotent") = forAll { (b: Set[Double]) ⇒ cast(Dyn(b, TySet(TyDouble)), TyI32) == None }
}

object DynamicMapSpec extends Properties("Dynamic lists") {
  property("Map idempotent") = forAll { (b: Map[String, Boolean]) ⇒ cast(Dyn(b, TyMap(TyString, TyBool)), TyMap(TyString, TyBool)) == Some(b) }
  property("Map idempotent") = forAll { (b: Map[String, Boolean]) ⇒ cast(Dyn(b, TyMap(TyString, TyBool)), TyMap(TyString, TyI32)) == None }
  property("Map idempotent") = forAll { (b: Map[String, Boolean]) ⇒ cast(Dyn(b, TyMap(TyString, TyBool)), TyMap(TyStruct, TyBool)) == None }
  property("Map idempotent") = forAll { (b: Map[String, Boolean]) ⇒ cast(Dyn(b, TyMap(TyString, TyBool)), TyBool) == None }
}

object DynamicStructSpec extends Properties("Dynamic lists") {
  val codecR = implicitly[TStructCodec[Rights]]
  property("Struct idempotent") = forAll { (b: Rights) ⇒ cast(Dyn(codecR.encode(b), TyStruct), TyStruct).flatMap(codecR.decode) == Some(b) }
  val codecC = implicitly[TStructCodec[Content]]
  property("Struct idempotent") = forAll { (b: Content) ⇒ cast(Dyn(codecC.encode(b), TyStruct), TyStruct).flatMap(codecC.decode) == Some(b) }
}
