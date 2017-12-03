package com.gu.story.model.v1

case class Content(id: String,`type`: ContentType) extends TStruct

object Content extends TStructCodec[Content]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,ContentType]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.id,w2.value -> x.`type`)

override def decode = (m) => for {
id <- m.get(w1.value).orElse(defaults.get(w1.value))

`type` <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield Content(id,`type`)
}