package com.gu.contentapi.client.model.v1

case class EmbedElementFields(
  html:          Option[String]  = None,
  safeEmbedCode: Option[Boolean] = None,
  alt:           Option[String]  = None,
  isMandatory:   Option[Boolean] = None) extends TStruct

object EmbedElementFields extends TStructCodec[EmbedElementFields] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, Option[Boolean]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[Boolean]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w3.value -> None,
    w4.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.html,
    w2.value -> x.safeEmbedCode,
    w3.value -> x.alt,
    w4.value -> x.isMandatory)
  override def decode = (m) ⇒ for {
    html ← m.get(w1.value).orElse(defaults.get(w1.value))
    safeEmbedCode ← m.get(w2.value).orElse(defaults.get(w2.value))
    alt ← m.get(w3.value).orElse(defaults.get(w3.value))
    isMandatory ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield EmbedElementFields(
    html,
    safeEmbedCode,
    alt,
    isMandatory)
}