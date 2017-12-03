package com.gu.contentapi.client.model.v1

case class InstagramElementFields(originalUrl: String,title: String,source: String,authorUrl: String,authorUsername: String,html: Option[String]= None,width: Option[Int]= None,alt: Option[String]= None,caption: Option[String]= None) extends TStruct

object InstagramElementFields extends TStructCodec[InstagramElementFields]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

val w6 = Witness(6)

val w7 = Witness(7)

val w8 = Witness(8)

val w9 = Witness(9)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,String]()

implicit val r3 = new TFieldCodec[w3.T,String]()

implicit val r4 = new TFieldCodec[w4.T,String]()

implicit val r5 = new TFieldCodec[w5.T,String]()

implicit val r6 = new TFieldCodec[w6.T,Option[String]]()

implicit val r7 = new TFieldCodec[w7.T,Option[Int]]()

implicit val r8 = new TFieldCodec[w8.T,Option[String]]()

implicit val r9 = new TFieldCodec[w9.T,Option[String]]()

override val defaults = HMap[TFieldCodec](w6.value -> None,w7.value -> None,w8.value -> None,w9.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.originalUrl,w2.value -> x.title,w3.value -> x.source,w4.value -> x.authorUrl,w5.value -> x.authorUsername,w6.value -> x.html,w7.value -> x.width,w8.value -> x.alt,w9.value -> x.caption)

override def decode = (m) => for {
originalUrl <- m.get(w1.value).orElse(defaults.get(w1.value))

title <- m.get(w2.value).orElse(defaults.get(w2.value))

source <- m.get(w3.value).orElse(defaults.get(w3.value))

authorUrl <- m.get(w4.value).orElse(defaults.get(w4.value))

authorUsername <- m.get(w5.value).orElse(defaults.get(w5.value))

html <- m.get(w6.value).orElse(defaults.get(w6.value))

width <- m.get(w7.value).orElse(defaults.get(w7.value))

alt <- m.get(w8.value).orElse(defaults.get(w8.value))

caption <- m.get(w9.value).orElse(defaults.get(w9.value))
} yield InstagramElementFields(originalUrl,title,source,authorUrl,authorUsername,html,width,alt,caption)
}