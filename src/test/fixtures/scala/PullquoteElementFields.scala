package com.gu.contentapi.client.model.v1

case class PullquoteElementFields(
  html:        Option[String] = None,
  attribution: Option[String] = None) extends TStruct

object PullquoteElementFields extends TStructCodec[PullquoteElementFields] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.html,
    w2.value -> x.attribution)
  override def decode = (m) ⇒ for {
    html ← m.get(w1.value).orElse(defaults.get(w1.value))
    attribution ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield PullquoteElementFields(
    html,
    attribution)
}