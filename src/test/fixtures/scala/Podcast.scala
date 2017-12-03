package com.gu.contentapi.client.model.v1

case class Podcast(linkUrl: String,copyright: String,author: String,subscriptionUrl: Option[String]= None,explicit: Boolean,image: Option[String]= None,categories: Option[List[PodcastCategory]]= None) extends TStruct

object Podcast extends TStructCodec[Podcast]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

val w6 = Witness(6)

val w7 = Witness(7)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,String]()

implicit val r3 = new TFieldCodec[w3.T,String]()

implicit val r4 = new TFieldCodec[w4.T,Option[String]]()

implicit val r5 = new TFieldCodec[w5.T,Boolean]()

implicit val r6 = new TFieldCodec[w6.T,Option[String]]()

implicit val r7 = new TFieldCodec[w7.T,Option[List[PodcastCategory]]]()

override val defaults = HMap[TFieldCodec](w4.value -> None,w6.value -> None,w7.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.linkUrl,w2.value -> x.copyright,w3.value -> x.author,w4.value -> x.subscriptionUrl,w5.value -> x.explicit,w6.value -> x.image,w7.value -> x.categories)

override def decode = (m) => for {
linkUrl <- m.get(w1.value).orElse(defaults.get(w1.value))

copyright <- m.get(w2.value).orElse(defaults.get(w2.value))

author <- m.get(w3.value).orElse(defaults.get(w3.value))

subscriptionUrl <- m.get(w4.value).orElse(defaults.get(w4.value))

explicit <- m.get(w5.value).orElse(defaults.get(w5.value))

image <- m.get(w6.value).orElse(defaults.get(w6.value))

categories <- m.get(w7.value).orElse(defaults.get(w7.value))
} yield Podcast(linkUrl,copyright,author,subscriptionUrl,explicit,image,categories)
}