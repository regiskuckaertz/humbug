package com.gu.contentapi.client.model.v1

case class VideoElementFields(url: Option[String] = None, description: Option[String] = None, title: Option[String] = None, html: Option[String] = None, source: Option[String] = None, credit: Option[String] = None, caption: Option[String] = None, height: Option[Int] = None, width: Option[Int] = None, duration: Option[Int] = None, contentAuthSystem: Option[String] = None, embeddable: Option[String] = None, isInappropriateForAdverts: Option[Boolean] = None, mediaId: Option[String] = None, stillImageUrl: Option[String] = None, thumbnailUrl: Option[String] = None, shortUrl: Option[String] = None, role: Option[String] = None, originalUrl: Option[String] = None) extends TStruct

object VideoElementFields extends TStructCodec[VideoElementFields] {
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

  val w15 = Witness(51)

  val w16 = Witness(61)

  val w17 = Witness(71)

  val w18 = Witness(81)

  val w19 = Witness(91)

  implicit val r1 = new TFieldCodec[w1.T, Option[String]]()

  implicit val r2 = new TFieldCodec[w2.T, Option[String]]()

  implicit val r3 = new TFieldCodec[w3.T, Option[String]]()

  implicit val r4 = new TFieldCodec[w4.T, Option[String]]()

  implicit val r5 = new TFieldCodec[w5.T, Option[String]]()

  implicit val r6 = new TFieldCodec[w6.T, Option[String]]()

  implicit val r7 = new TFieldCodec[w7.T, Option[String]]()

  implicit val r8 = new TFieldCodec[w8.T, Option[Int]]()

  implicit val r9 = new TFieldCodec[w9.T, Option[Int]]()

  implicit val r10 = new TFieldCodec[w10.T, Option[Int]]()

  implicit val r11 = new TFieldCodec[w11.T, Option[String]]()

  implicit val r12 = new TFieldCodec[w12.T, Option[String]]()

  implicit val r13 = new TFieldCodec[w13.T, Option[Boolean]]()

  implicit val r14 = new TFieldCodec[w14.T, Option[String]]()

  implicit val r15 = new TFieldCodec[w15.T, Option[String]]()

  implicit val r16 = new TFieldCodec[w16.T, Option[String]]()

  implicit val r17 = new TFieldCodec[w17.T, Option[String]]()

  implicit val r18 = new TFieldCodec[w18.T, Option[String]]()

  implicit val r19 = new TFieldCodec[w19.T, Option[String]]()

  override val defaults = HMap[TFieldCodec](w1.value -> None, w2.value -> None, w3.value -> None, w4.value -> None, w5.value -> None, w6.value -> None, w7.value -> None, w8.value -> None, w9.value -> None, w10.value -> None, w11.value -> None, w12.value -> None, w13.value -> None, w14.value -> None, w15.value -> None, w16.value -> None, w17.value -> None, w18.value -> None, w19.value -> None)

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.url, w2.value -> x.description, w3.value -> x.title, w4.value -> x.html, w5.value -> x.source, w6.value -> x.credit, w7.value -> x.caption, w8.value -> x.height, w9.value -> x.width, w10.value -> x.duration, w11.value -> x.contentAuthSystem, w12.value -> x.embeddable, w13.value -> x.isInappropriateForAdverts, w14.value -> x.mediaId, w15.value -> x.stillImageUrl, w16.value -> x.thumbnailUrl, w17.value -> x.shortUrl, w18.value -> x.role, w19.value -> x.originalUrl)

  override def decode = (m) ⇒ for {
    url ← m.get(w1.value).orElse(defaults.get(w1.value))

    description ← m.get(w2.value).orElse(defaults.get(w2.value))

    title ← m.get(w3.value).orElse(defaults.get(w3.value))

    html ← m.get(w4.value).orElse(defaults.get(w4.value))

    source ← m.get(w5.value).orElse(defaults.get(w5.value))

    credit ← m.get(w6.value).orElse(defaults.get(w6.value))

    caption ← m.get(w7.value).orElse(defaults.get(w7.value))

    height ← m.get(w8.value).orElse(defaults.get(w8.value))

    width ← m.get(w9.value).orElse(defaults.get(w9.value))

    duration ← m.get(w10.value).orElse(defaults.get(w10.value))

    contentAuthSystem ← m.get(w11.value).orElse(defaults.get(w11.value))

    embeddable ← m.get(w12.value).orElse(defaults.get(w12.value))

    isInappropriateForAdverts ← m.get(w13.value).orElse(defaults.get(w13.value))

    mediaId ← m.get(w14.value).orElse(defaults.get(w14.value))

    stillImageUrl ← m.get(w15.value).orElse(defaults.get(w15.value))

    thumbnailUrl ← m.get(w16.value).orElse(defaults.get(w16.value))

    shortUrl ← m.get(w17.value).orElse(defaults.get(w17.value))

    role ← m.get(w18.value).orElse(defaults.get(w18.value))

    originalUrl ← m.get(w19.value).orElse(defaults.get(w19.value))
  } yield VideoElementFields(url, description, title, html, source, credit, caption, height, width, duration, contentAuthSystem, embeddable, isInappropriateForAdverts, mediaId, stillImageUrl, thumbnailUrl, shortUrl, role, originalUrl)
}