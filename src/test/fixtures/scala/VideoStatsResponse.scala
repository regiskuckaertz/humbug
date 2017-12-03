package com.gu.contentapi.client.model.v1

case class VideoStatsResponse(
  status:           String,
  mostViewedVideos: List[MostViewedVideo]) extends TStruct

object VideoStatsResponse extends TStructCodec[VideoStatsResponse] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, List[MostViewedVideo]]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.status,
    w2.value -> x.mostViewedVideos)
  override def decode = (m) ⇒ for {
    status ← m.get(w1.value).orElse(defaults.get(w1.value))
    mostViewedVideos ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield VideoStatsResponse(
    status,
    mostViewedVideos)
}