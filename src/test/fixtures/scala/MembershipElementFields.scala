package com.gu.contentapi.client.model.v1

case class MembershipElementFields(originalUrl: Option[String]= None,linkText: Option[String]= None,linkPrefix: Option[String]= None,title: Option[String]= None,venue: Option[String]= None,location: Option[String]= None,identifier: Option[String]= None,image: Option[String]= None,price: Option[String]= None,start: Option[CapiDateTime]= None,end: Option[CapiDateTime]= None) extends TStruct

object MembershipElementFields extends TStructCodec[MembershipElementFields]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

val w6 = Witness(6)

val w7 = Witness(7)

val w8 = Witness(8)

val w9 = Witness(9)

val w10 = Witness(1)

val w11 = Witness(11)

implicit val r1 = new TFieldCodec[w1.T,Option[String]]()

implicit val r2 = new TFieldCodec[w2.T,Option[String]]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

implicit val r4 = new TFieldCodec[w4.T,Option[String]]()

implicit val r5 = new TFieldCodec[w5.T,Option[String]]()

implicit val r6 = new TFieldCodec[w6.T,Option[String]]()

implicit val r7 = new TFieldCodec[w7.T,Option[String]]()

implicit val r8 = new TFieldCodec[w8.T,Option[String]]()

implicit val r9 = new TFieldCodec[w9.T,Option[String]]()

implicit val r10 = new TFieldCodec[w10.T,Option[CapiDateTime]]()

implicit val r11 = new TFieldCodec[w11.T,Option[CapiDateTime]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w2.value -> None,w3.value -> None,w4.value -> None,w5.value -> None,w6.value -> None,w7.value -> None,w8.value -> None,w9.value -> None,w10.value -> None,w11.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.originalUrl,w2.value -> x.linkText,w3.value -> x.linkPrefix,w4.value -> x.title,w5.value -> x.venue,w6.value -> x.location,w7.value -> x.identifier,w8.value -> x.image,w9.value -> x.price,w10.value -> x.start,w11.value -> x.end)

override def decode = (m) => for {
originalUrl <- m.get(w1.value).orElse(defaults.get(w1.value))

linkText <- m.get(w2.value).orElse(defaults.get(w2.value))

linkPrefix <- m.get(w3.value).orElse(defaults.get(w3.value))

title <- m.get(w4.value).orElse(defaults.get(w4.value))

venue <- m.get(w5.value).orElse(defaults.get(w5.value))

location <- m.get(w6.value).orElse(defaults.get(w6.value))

identifier <- m.get(w7.value).orElse(defaults.get(w7.value))

image <- m.get(w8.value).orElse(defaults.get(w8.value))

price <- m.get(w9.value).orElse(defaults.get(w9.value))

start <- m.get(w10.value).orElse(defaults.get(w10.value))

end <- m.get(w11.value).orElse(defaults.get(w11.value))
} yield MembershipElementFields(originalUrl,linkText,linkPrefix,title,venue,location,identifier,image,price,start,end)
}