package com.gu.contententity.thrift

case class Price(currency: String,value: Int) extends TStruct

object Price extends TStructCodec[Price]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,Int]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.currency,w2.value -> x.value)

override def decode = (m) => for {
currency <- m.get(w1.value).orElse(defaults.get(w1.value))

value <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield Price(currency,value)
}