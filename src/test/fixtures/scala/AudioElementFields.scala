package com.gu.contentapi.client.model.v1

case class AudioElementFields(html: Option[String]= None,source: Option[String]= None,description: Option[String]= None,title: Option[String]= None,credit: Option[String]= None,caption: Option[String]= None,durationMinutes: Option[Int]= None,durationSeconds: Option[Int]= None,clean: Option[Boolean]= None,explicit: Option[Boolean]= None) extends TStruct

object AudioElementFields extends TStructCodec[AudioElementFields]{
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

implicit val r1 = new TFieldCodec[w1.T,Option[String]]()

implicit val r2 = new TFieldCodec[w2.T,Option[String]]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

implicit val r4 = new TFieldCodec[w4.T,Option[String]]()

implicit val r5 = new TFieldCodec[w5.T,Option[String]]()

implicit val r6 = new TFieldCodec[w6.T,Option[String]]()

implicit val r7 = new TFieldCodec[w7.T,Option[Int]]()

implicit val r8 = new TFieldCodec[w8.T,Option[Int]]()

implicit val r9 = new TFieldCodec[w9.T,Option[Boolean]]()

implicit val r10 = new TFieldCodec[w10.T,Option[Boolean]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w2.value -> None,w3.value -> None,w4.value -> None,w5.value -> None,w6.value -> None,w7.value -> None,w8.value -> None,w9.value -> None,w10.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.html,w2.value -> x.source,w3.value -> x.description,w4.value -> x.title,w5.value -> x.credit,w6.value -> x.caption,w7.value -> x.durationMinutes,w8.value -> x.durationSeconds,w9.value -> x.clean,w10.value -> x.explicit)

override def decode = (m) => for {
html <- m.get(w1.value).orElse(defaults.get(w1.value))

source <- m.get(w2.value).orElse(defaults.get(w2.value))

description <- m.get(w3.value).orElse(defaults.get(w3.value))

title <- m.get(w4.value).orElse(defaults.get(w4.value))

credit <- m.get(w5.value).orElse(defaults.get(w5.value))

caption <- m.get(w6.value).orElse(defaults.get(w6.value))

durationMinutes <- m.get(w7.value).orElse(defaults.get(w7.value))

durationSeconds <- m.get(w8.value).orElse(defaults.get(w8.value))

clean <- m.get(w9.value).orElse(defaults.get(w9.value))

explicit <- m.get(w10.value).orElse(defaults.get(w10.value))
} yield AudioElementFields(html,source,description,title,credit,caption,durationMinutes,durationSeconds,clean,explicit)
}