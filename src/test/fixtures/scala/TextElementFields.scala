package com.gu.contentapi.client.model.v1

case class TextElementFields(html: Option[String] = None) extends TStruct

object TextElementFields extends TStructCodec[TextElementFields] {
  val w1 = Witness(1)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  override val defaults = HMap[TFieldCodec](w1.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.html)
  override def decode = (m) ⇒ for {
    html ← m.get(w1.value).orElse(defaults.get(w1.value))
  } yield TextElementFields(html)
}