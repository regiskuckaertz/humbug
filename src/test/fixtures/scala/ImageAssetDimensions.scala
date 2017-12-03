package com.gu.contentatom.thrift

case class ImageAssetDimensions(height: Int,width: Int) extends TStruct

object ImageAssetDimensions extends TStructCodec[ImageAssetDimensions]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,Int]()

implicit val r2 = new TFieldCodec[w2.T,Int]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.height,w2.value -> x.width)

override def decode = (m) => for {
height <- m.get(w1.value).orElse(defaults.get(w1.value))

width <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield ImageAssetDimensions(height,width)
}