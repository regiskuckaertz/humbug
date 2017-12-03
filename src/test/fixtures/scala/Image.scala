package com.gu.contentatom.thrift

case class Image(assets: List[ImageAsset],master: Option[ImageAsset]= None,mediaId: String,source: Option[String]= None) extends TStruct

object Image extends TStructCodec[Image]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

implicit val r1 = new TFieldCodec[w1.T,List[ImageAsset]]()

implicit val r2 = new TFieldCodec[w2.T,Option[ImageAsset]]()

implicit val r3 = new TFieldCodec[w3.T,String]()

implicit val r4 = new TFieldCodec[w4.T,Option[String]]()

override val defaults = HMap[TFieldCodec](w2.value -> None,w4.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.assets,w2.value -> x.master,w3.value -> x.mediaId,w4.value -> x.source)

override def decode = (m) => for {
assets <- m.get(w1.value).orElse(defaults.get(w1.value))

master <- m.get(w2.value).orElse(defaults.get(w2.value))

mediaId <- m.get(w3.value).orElse(defaults.get(w3.value))

source <- m.get(w4.value).orElse(defaults.get(w4.value))
} yield Image(assets,master,mediaId,source)
}