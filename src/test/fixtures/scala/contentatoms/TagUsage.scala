package com.gu.contentatom.thrift

case class TagUsage(tag: Tag,isLead: Boolean= `false`) extends TStruct

object TagUsage extends TStructCodec[TagUsage]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,Tag]()

implicit val r2 = new TFieldCodec[w2.T,Boolean]()

override val defaults = HMap[TFieldCodec](w2.value -> `false`)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.tag,w2.value -> x.isLead)

override def decode = (m) => for {
tag <- m.get(w1.value).orElse(defaults.get(w1.value))

isLead <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield TagUsage(tag,isLead)
}