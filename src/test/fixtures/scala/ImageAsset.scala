package com.gu.contentatom.thrift

case class ImageAsset(mimeType: Option[String]= None,file: String,dimensions: Option[ImageAssetDimensions]= None,size: Option[Long]= None,aspectRatio: Option[String]= None,credit: Option[String]= None) extends TStruct

object ImageAsset extends TStructCodec[ImageAsset]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

val w6 = Witness(6)

implicit val r1 = new TFieldCodec[w1.T,Option[String]]()

implicit val r2 = new TFieldCodec[w2.T,String]()

implicit val r3 = new TFieldCodec[w3.T,Option[ImageAssetDimensions]]()

implicit val r4 = new TFieldCodec[w4.T,Option[Long]]()

implicit val r5 = new TFieldCodec[w5.T,Option[String]]()

implicit val r6 = new TFieldCodec[w6.T,Option[String]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w3.value -> None,w4.value -> None,w5.value -> None,w6.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.mimeType,w2.value -> x.file,w3.value -> x.dimensions,w4.value -> x.size,w5.value -> x.aspectRatio,w6.value -> x.credit)

override def decode = (m) => for {
mimeType <- m.get(w1.value).orElse(defaults.get(w1.value))

file <- m.get(w2.value).orElse(defaults.get(w2.value))

dimensions <- m.get(w3.value).orElse(defaults.get(w3.value))

size <- m.get(w4.value).orElse(defaults.get(w4.value))

aspectRatio <- m.get(w5.value).orElse(defaults.get(w5.value))

credit <- m.get(w6.value).orElse(defaults.get(w6.value))
} yield ImageAsset(mimeType,file,dimensions,size,aspectRatio,credit)
}