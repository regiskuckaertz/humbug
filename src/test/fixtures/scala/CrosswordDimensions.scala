package com.gu.contentapi.client.model.v1

case class CrosswordDimensions(cols: Int,rows: Int) extends TStruct

object CrosswordDimensions extends TStructCodec[CrosswordDimensions]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,Int]()

implicit val r2 = new TFieldCodec[w2.T,Int]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.cols,w2.value -> x.rows)

override def decode = (m) => for {
cols <- m.get(w1.value).orElse(defaults.get(w1.value))

rows <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield CrosswordDimensions(cols,rows)
}