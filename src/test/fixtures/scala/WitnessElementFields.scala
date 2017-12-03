package com.gu.contentapi.client.model.v1

case class WitnessElementFields(
  url:                      Option[String]       = None,
  originalUrl:              Option[String]       = None,
  witnessEmbedType:         Option[String]       = None,
  mediaId:                  Option[String]       = None,
  source:                   Option[String]       = None,
  title:                    Option[String]       = None,
  description:              Option[String]       = None,
  authorName:               Option[String]       = None,
  authorUsername:           Option[String]       = None,
  authorWitnessProfileUrl:  Option[String]       = None,
  authorGuardianProfileUrl: Option[String]       = None,
  caption:                  Option[String]       = None,
  alt:                      Option[String]       = None,
  width:                    Option[Int]          = None,
  height:                   Option[Int]          = None,
  html:                     Option[String]       = None,
  apiUrl:                   Option[String]       = None,
  photographer:             Option[String]       = None,
  dateCreated:              Option[CapiDateTime] = None,
  youtubeUrl:               Option[String]       = None,
  youtubeSource:            Option[String]       = None,
  youtubeTitle:             Option[String]       = None,
  youtubeDescription:       Option[String]       = None,
  youtubeAuthorName:        Option[String]       = None,
  youtubeHtml:              Option[String]       = None,
  role:                     Option[String]       = None) extends TStruct

object WitnessElementFields extends TStructCodec[WitnessElementFields] {
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
  val w20 = Witness(2)
  val w21 = Witness(12)
  val w22 = Witness(22)
  val w23 = Witness(32)
  val w24 = Witness(42)
  val w25 = Witness(52)
  val w26 = Witness(62)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  implicit val r5 = new TFieldCodec[w5.T, Option[String]]
  implicit val r6 = new TFieldCodec[w6.T, Option[String]]
  implicit val r7 = new TFieldCodec[w7.T, Option[String]]
  implicit val r8 = new TFieldCodec[w8.T, Option[String]]
  implicit val r9 = new TFieldCodec[w9.T, Option[String]]
  implicit val r10 = new TFieldCodec[w10.T, Option[String]]
  implicit val r11 = new TFieldCodec[w11.T, Option[String]]
  implicit val r12 = new TFieldCodec[w12.T, Option[String]]
  implicit val r13 = new TFieldCodec[w13.T, Option[String]]
  implicit val r14 = new TFieldCodec[w14.T, Option[Int]]
  implicit val r15 = new TFieldCodec[w15.T, Option[Int]]
  implicit val r16 = new TFieldCodec[w16.T, Option[String]]
  implicit val r17 = new TFieldCodec[w17.T, Option[String]]
  implicit val r18 = new TFieldCodec[w18.T, Option[String]]
  implicit val r19 = new TFieldCodec[w19.T, Option[CapiDateTime]]
  implicit val r20 = new TFieldCodec[w20.T, Option[String]]
  implicit val r21 = new TFieldCodec[w21.T, Option[String]]
  implicit val r22 = new TFieldCodec[w22.T, Option[String]]
  implicit val r23 = new TFieldCodec[w23.T, Option[String]]
  implicit val r24 = new TFieldCodec[w24.T, Option[String]]
  implicit val r25 = new TFieldCodec[w25.T, Option[String]]
  implicit val r26 = new TFieldCodec[w26.T, Option[String]]
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
    w12.value -> None,
    w13.value -> None,
    w14.value -> None,
    w15.value -> None,
    w16.value -> None,
    w17.value -> None,
    w18.value -> None,
    w19.value -> None,
    w20.value -> None,
    w21.value -> None,
    w22.value -> None,
    w23.value -> None,
    w24.value -> None,
    w25.value -> None,
    w26.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.url,
    w2.value -> x.originalUrl,
    w3.value -> x.witnessEmbedType,
    w4.value -> x.mediaId,
    w5.value -> x.source,
    w6.value -> x.title,
    w7.value -> x.description,
    w8.value -> x.authorName,
    w9.value -> x.authorUsername,
    w10.value -> x.authorWitnessProfileUrl,
    w11.value -> x.authorGuardianProfileUrl,
    w12.value -> x.caption,
    w13.value -> x.alt,
    w14.value -> x.width,
    w15.value -> x.height,
    w16.value -> x.html,
    w17.value -> x.apiUrl,
    w18.value -> x.photographer,
    w19.value -> x.dateCreated,
    w20.value -> x.youtubeUrl,
    w21.value -> x.youtubeSource,
    w22.value -> x.youtubeTitle,
    w23.value -> x.youtubeDescription,
    w24.value -> x.youtubeAuthorName,
    w25.value -> x.youtubeHtml,
    w26.value -> x.role)
  override def decode = (m) ⇒ for {
    url ← m.get(w1.value).orElse(defaults.get(w1.value))
    originalUrl ← m.get(w2.value).orElse(defaults.get(w2.value))
    witnessEmbedType ← m.get(w3.value).orElse(defaults.get(w3.value))
    mediaId ← m.get(w4.value).orElse(defaults.get(w4.value))
    source ← m.get(w5.value).orElse(defaults.get(w5.value))
    title ← m.get(w6.value).orElse(defaults.get(w6.value))
    description ← m.get(w7.value).orElse(defaults.get(w7.value))
    authorName ← m.get(w8.value).orElse(defaults.get(w8.value))
    authorUsername ← m.get(w9.value).orElse(defaults.get(w9.value))
    authorWitnessProfileUrl ← m.get(w10.value).orElse(defaults.get(w10.value))
    authorGuardianProfileUrl ← m.get(w11.value).orElse(defaults.get(w11.value))
    caption ← m.get(w12.value).orElse(defaults.get(w12.value))
    alt ← m.get(w13.value).orElse(defaults.get(w13.value))
    width ← m.get(w14.value).orElse(defaults.get(w14.value))
    height ← m.get(w15.value).orElse(defaults.get(w15.value))
    html ← m.get(w16.value).orElse(defaults.get(w16.value))
    apiUrl ← m.get(w17.value).orElse(defaults.get(w17.value))
    photographer ← m.get(w18.value).orElse(defaults.get(w18.value))
    dateCreated ← m.get(w19.value).orElse(defaults.get(w19.value))
    youtubeUrl ← m.get(w20.value).orElse(defaults.get(w20.value))
    youtubeSource ← m.get(w21.value).orElse(defaults.get(w21.value))
    youtubeTitle ← m.get(w22.value).orElse(defaults.get(w22.value))
    youtubeDescription ← m.get(w23.value).orElse(defaults.get(w23.value))
    youtubeAuthorName ← m.get(w24.value).orElse(defaults.get(w24.value))
    youtubeHtml ← m.get(w25.value).orElse(defaults.get(w25.value))
    role ← m.get(w26.value).orElse(defaults.get(w26.value))
  } yield WitnessElementFields(
    url,
    originalUrl,
    witnessEmbedType,
    mediaId,
    source,
    title,
    description,
    authorName,
    authorUsername,
    authorWitnessProfileUrl,
    authorGuardianProfileUrl,
    caption,
    alt,
    width,
    height,
    html,
    apiUrl,
    photographer,
    dateCreated,
    youtubeUrl,
    youtubeSource,
    youtubeTitle,
    youtubeDescription,
    youtubeAuthorName,
    youtubeHtml,
    role)
}