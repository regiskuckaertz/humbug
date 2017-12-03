package com.gu.contentapi.client.model.v1

case class CrosswordCreator(
  name:   String,
  webUrl: String) extends TStruct

object CrosswordCreator extends TStructCodec[CrosswordCreator] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.name,
    w2.value -> x.webUrl)
  override def decode = (m) ⇒ for {
    name ← m.get(w1.value).orElse(defaults.get(w1.value))
    webUrl ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield CrosswordCreator(
    name,
    webUrl)
}