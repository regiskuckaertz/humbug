package com.gu.contentatom.thrift

case class Flags(
  legallySensitive: Option[Boolean] = None,
  blockAds:         Option[Boolean] = None,
  sensitive:        Option[Boolean] = None) extends TStruct

object Flags extends TStructCodec[Flags] {
  val w1 = Witness(2)
  val w2 = Witness(3)
  val w3 = Witness(4)
  implicit val r1 = new TFieldCodec[w1.T, Option[Boolean]]
  implicit val r2 = new TFieldCodec[w2.T, Option[Boolean]]
  implicit val r3 = new TFieldCodec[w3.T, Option[Boolean]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w3.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.legallySensitive,
    w2.value -> x.blockAds,
    w3.value -> x.sensitive)
  override def decode = (m) ⇒ for {
    legallySensitive ← m.get(w1.value).orElse(defaults.get(w1.value))
    blockAds ← m.get(w2.value).orElse(defaults.get(w2.value))
    sensitive ← m.get(w3.value).orElse(defaults.get(w3.value))
  } yield Flags(
    legallySensitive,
    blockAds,
    sensitive)
}