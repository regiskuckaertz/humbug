package com.gu.contentapi.client.model.v1

case class ContentAtomElementFields(
  atomId:   String,
  atomType: String) extends TStruct

object ContentAtomElementFields extends TStructCodec[ContentAtomElementFields] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.atomId,
    w2.value -> x.atomType)
  override def decode = (m) ⇒ for {
    atomId ← m.get(w1.value).orElse(defaults.get(w1.value))
    atomType ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield ContentAtomElementFields(
    atomId,
    atomType)
}