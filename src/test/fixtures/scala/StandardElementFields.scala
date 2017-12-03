package com.gu.contentapi.client.model.v1

case class StandardElementFields(
  url:         Option[String]  = None,
  originalUrl: Option[String]  = None,
  source:      Option[String]  = None,
  title:       Option[String]  = None,
  description: Option[String]  = None,
  credit:      Option[String]  = None,
  caption:     Option[String]  = None,
  width:       Option[Int]     = None,
  height:      Option[Int]     = None,
  html:        Option[String]  = None,
  role:        Option[String]  = None,
  isMandatory: Option[Boolean] = None) extends TStruct

object StandardElementFields extends TStructCodec[StandardElementFields] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  val w6 = Witness(6)
  val w7 = Witness(7)
  val w8 = Witness(8)
  val w9 = Witness(9)
  val w10 = Witness(1)
  val w11 = Witness(11)
  val w12 = Witness(21)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  implicit val r5 = new TFieldCodec[w5.T, Option[String]]
  implicit val r6 = new TFieldCodec[w6.T, Option[String]]
  implicit val r7 = new TFieldCodec[w7.T, Option[String]]
  implicit val r8 = new TFieldCodec[w8.T, Option[Int]]
  implicit val r9 = new TFieldCodec[w9.T, Option[Int]]
  implicit val r10 = new TFieldCodec[w10.T, Option[String]]
  implicit val r11 = new TFieldCodec[w11.T, Option[String]]
  implicit val r12 = new TFieldCodec[w12.T, Option[Boolean]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w3.value -> None,
    w4.value -> None,
    w5.value -> None,
    w6.value -> None,
    w7.value -> None,
    w8.value -> None,
    w9.value -> None,
    w10.value -> None,
    w11.value -> None,
    w12.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.url,
    w2.value -> x.originalUrl,
    w3.value -> x.source,
    w4.value -> x.title,
    w5.value -> x.description,
    w6.value -> x.credit,
    w7.value -> x.caption,
    w8.value -> x.width,
    w9.value -> x.height,
    w10.value -> x.html,
    w11.value -> x.role,
    w12.value -> x.isMandatory)
  override def decode = (m) ⇒ for {
    url ← m.get(w1.value).orElse(defaults.get(w1.value))
    originalUrl ← m.get(w2.value).orElse(defaults.get(w2.value))
    source ← m.get(w3.value).orElse(defaults.get(w3.value))
    title ← m.get(w4.value).orElse(defaults.get(w4.value))
    description ← m.get(w5.value).orElse(defaults.get(w5.value))
    credit ← m.get(w6.value).orElse(defaults.get(w6.value))
    caption ← m.get(w7.value).orElse(defaults.get(w7.value))
    width ← m.get(w8.value).orElse(defaults.get(w8.value))
    height ← m.get(w9.value).orElse(defaults.get(w9.value))
    html ← m.get(w10.value).orElse(defaults.get(w10.value))
    role ← m.get(w11.value).orElse(defaults.get(w11.value))
    isMandatory ← m.get(w12.value).orElse(defaults.get(w12.value))
  } yield StandardElementFields(
    url,
    originalUrl,
    source,
    title,
    description,
    credit,
    caption,
    width,
    height,
    html,
    role,
    isMandatory)
}