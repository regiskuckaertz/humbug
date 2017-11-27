package com.gu.contentapi.client.model.v1

case class MostViewedVideo(id: String, count: Int) extends TStruct

object MostViewedVideo extends TStructCodec[MostViewedVideo] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  implicit val r1 = new TFieldCodec[w1.T, String]()

  implicit val r2 = new TFieldCodec[w2.T, Int]()

  override val defaults = HMap[TFieldCodec]()

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.id, w2.value -> x.count)

  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))

    count ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield MostViewedVideo(id, count)
}