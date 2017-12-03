package com.gu.contentapi.client.model.v1

case class PackageArticle(metadata: Article,content: Content) extends TStruct

object PackageArticle extends TStructCodec[PackageArticle]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,Article]()

implicit val r2 = new TFieldCodec[w2.T,Content]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.metadata,w2.value -> x.content)

override def decode = (m) => for {
metadata <- m.get(w1.value).orElse(defaults.get(w1.value))

content <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield PackageArticle(metadata,content)
}