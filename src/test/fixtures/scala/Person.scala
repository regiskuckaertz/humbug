package com.gu.contententity.thrift.entity.person

case class Person(fullName: String) extends TStruct

object Person extends TStructCodec[Person]{
val w1 = Witness(1)

implicit val r1 = new TFieldCodec[w1.T,String]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.fullName)

override def decode = (m) => for {
fullName <- m.get(w1.value).orElse(defaults.get(w1.value))
} yield Person(fullName)
}