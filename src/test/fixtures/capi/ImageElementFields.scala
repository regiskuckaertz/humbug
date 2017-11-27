package com.gu.contentapi.client.model.v1

case class ImageElementFields(caption: Option[String] = None, copyright: Option[String] = None, displayCredit: Option[Boolean] = None, credit: Option[String] = None, source: Option[String] = None, photographer: Option[String] = None, alt: Option[String] = None, mediaId: Option[String] = None, mediaApiUri: Option[String] = None, picdarUrn: Option[String] = None, suppliersReference: Option[String] = None, imageType: Option[String] = None, comment: Option[String] = None, role: Option[String] = None) extends TStruct

object ImageElementFields extends TStructCodec[ImageElementFields] {
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

  val w13 = Witness(31)

  val w14 = Witness(41)

  implicit val r1 = new TFieldCodec[w1.T, Option[String]]()

  implicit val r2 = new TFieldCodec[w2.T, Option[String]]()

  implicit val r3 = new TFieldCodec[w3.T, Option[Boolean]]()

  implicit val r4 = new TFieldCodec[w4.T, Option[String]]()

  implicit val r5 = new TFieldCodec[w5.T, Option[String]]()

  implicit val r6 = new TFieldCodec[w6.T, Option[String]]()

  implicit val r7 = new TFieldCodec[w7.T, Option[String]]()

  implicit val r8 = new TFieldCodec[w8.T, Option[String]]()

  implicit val r9 = new TFieldCodec[w9.T, Option[String]]()

  implicit val r10 = new TFieldCodec[w10.T, Option[String]]()

  implicit val r11 = new TFieldCodec[w11.T, Option[String]]()

  implicit val r12 = new TFieldCodec[w12.T, Option[String]]()

  implicit val r13 = new TFieldCodec[w13.T, Option[String]]()

  implicit val r14 = new TFieldCodec[w14.T, Option[String]]()

  override val defaults = HMap[TFieldCodec](w1.value -> None, w2.value -> None, w3.value -> None, w4.value -> None, w5.value -> None, w6.value -> None, w7.value -> None, w8.value -> None, w9.value -> None, w10.value -> None, w11.value -> None, w12.value -> None, w13.value -> None, w14.value -> None)

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.caption, w2.value -> x.copyright, w3.value -> x.displayCredit, w4.value -> x.credit, w5.value -> x.source, w6.value -> x.photographer, w7.value -> x.alt, w8.value -> x.mediaId, w9.value -> x.mediaApiUri, w10.value -> x.picdarUrn, w11.value -> x.suppliersReference, w12.value -> x.imageType, w13.value -> x.comment, w14.value -> x.role)

  override def decode = (m) ⇒ for {
    caption ← m.get(w1.value).orElse(defaults.get(w1.value))

    copyright ← m.get(w2.value).orElse(defaults.get(w2.value))

    displayCredit ← m.get(w3.value).orElse(defaults.get(w3.value))

    credit ← m.get(w4.value).orElse(defaults.get(w4.value))

    source ← m.get(w5.value).orElse(defaults.get(w5.value))

    photographer ← m.get(w6.value).orElse(defaults.get(w6.value))

    alt ← m.get(w7.value).orElse(defaults.get(w7.value))

    mediaId ← m.get(w8.value).orElse(defaults.get(w8.value))

    mediaApiUri ← m.get(w9.value).orElse(defaults.get(w9.value))

    picdarUrn ← m.get(w10.value).orElse(defaults.get(w10.value))

    suppliersReference ← m.get(w11.value).orElse(defaults.get(w11.value))

    imageType ← m.get(w12.value).orElse(defaults.get(w12.value))

    comment ← m.get(w13.value).orElse(defaults.get(w13.value))

    role ← m.get(w14.value).orElse(defaults.get(w14.value))
  } yield ImageElementFields(caption, copyright, displayCredit, credit, source, photographer, alt, mediaId, mediaApiUri, picdarUrn, suppliersReference, imageType, comment, role)
}