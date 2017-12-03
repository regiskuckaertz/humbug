package com.gu.contententity.thrift

case class Geolocation(
  lat: Double,
  lon: Double) extends TStruct

object Geolocation extends TStructCodec[Geolocation] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, Double]
  implicit val r2 = new TFieldCodec[w2.T, Double]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.lat,
    w2.value -> x.lon)
  override def decode = (m) ⇒ for {
    lat ← m.get(w1.value).orElse(defaults.get(w1.value))
    lon ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield Geolocation(
    lat,
    lon)
}