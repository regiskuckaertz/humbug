package com.gu.contentapi.client.model.v1

case class RichLinkElementFields(url: Option[String]= None,originalUrl: Option[String]= None,linkText: Option[String]= None,linkPrefix: Option[String]= None,role: Option[String]= None) extends TStruct

object RichLinkElementFields extends TStructCodec[RichLinkElementFields]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

implicit val r1 = new TFieldCodec[w1.T,Option[String]]()

implicit val r2 = new TFieldCodec[w2.T,Option[String]]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

implicit val r4 = new TFieldCodec[w4.T,Option[String]]()

implicit val r5 = new TFieldCodec[w5.T,Option[String]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w2.value -> None,w3.value -> None,w4.value -> None,w5.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.url,w2.value -> x.originalUrl,w3.value -> x.linkText,w4.value -> x.linkPrefix,w5.value -> x.role)

override def decode = (m) => for {
url <- m.get(w1.value).orElse(defaults.get(w1.value))

originalUrl <- m.get(w2.value).orElse(defaults.get(w2.value))

linkText <- m.get(w3.value).orElse(defaults.get(w3.value))

linkPrefix <- m.get(w4.value).orElse(defaults.get(w4.value))

role <- m.get(w5.value).orElse(defaults.get(w5.value))
} yield RichLinkElementFields(url,originalUrl,linkText,linkPrefix,role)
}