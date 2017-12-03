package com.gu.contentatom.thrift

case class Newspaper(book: Tag,bookSection: Tag,publication: Tag) extends TStruct

object Newspaper extends TStructCodec[Newspaper]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

implicit val r1 = new TFieldCodec[w1.T,Tag]()

implicit val r2 = new TFieldCodec[w2.T,Tag]()

implicit val r3 = new TFieldCodec[w3.T,Tag]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.book,w2.value -> x.bookSection,w3.value -> x.publication)

override def decode = (m) => for {
book <- m.get(w1.value).orElse(defaults.get(w1.value))

bookSection <- m.get(w2.value).orElse(defaults.get(w2.value))

publication <- m.get(w3.value).orElse(defaults.get(w3.value))
} yield Newspaper(book,bookSection,publication)
}