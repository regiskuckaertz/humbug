package com.gu.story.model.v1

case class StoryEvent(id: String,name: String,summary: String,content: List[Content]) extends TStruct

object StoryEvent extends TStructCodec[StoryEvent]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,String]()

implicit val r3 = new TFieldCodec[w3.T,String]()

implicit val r4 = new TFieldCodec[w4.T,List[Content]]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.id,w2.value -> x.name,w3.value -> x.summary,w4.value -> x.content)

override def decode = (m) => for {
id <- m.get(w1.value).orElse(defaults.get(w1.value))

name <- m.get(w2.value).orElse(defaults.get(w2.value))

summary <- m.get(w3.value).orElse(defaults.get(w3.value))

content <- m.get(w4.value).orElse(defaults.get(w4.value))
} yield StoryEvent(id,name,summary,content)
}