package com.gu.contentatom.thrift.atom.quiz

case class Asset(
  `type`: String,
  data:   OpaqueJson) extends TStruct

object Asset extends TStructCodec[Asset] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, OpaqueJson]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.`type`,
    w2.value -> x.data)
  override def decode = (m) ⇒ for {
    `type` ← m.get(w1.value).orElse(defaults.get(w1.value))
    data ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield Asset(
    `type`,
    data)
}