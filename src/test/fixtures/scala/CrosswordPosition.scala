package com.gu.contentapi.client.model.v1

case class CrosswordPosition(x: Int,y: Int) extends TStruct

object CrosswordPosition extends TStructCodec[CrosswordPosition]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,Int]()

implicit val r2 = new TFieldCodec[w2.T,Int]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.x,w2.value -> x.y)

override def decode = (m) => for {
x <- m.get(w1.value).orElse(defaults.get(w1.value))

y <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield CrosswordPosition(x,y)
}