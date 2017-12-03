package com.gu.contentapi.client.model.v1

case class Content(
  id:                 String,
  `type`:             ContentType           = ContentType.ARTICLE,
  sectionId:          Option[String]        = None,
  sectionName:        Option[String]        = None,
  webPublicationDate: Option[CapiDateTime]  = None,
  webTitle:           String,
  webUrl:             String,
  apiUrl:             String,
  fields:             Option[ContentFields] = None,
  tags:               List[Tag]             = List,
  elements:           Option[List[Element]] = None,
  references:         List[Reference]       = List,
  isExpired:          Option[Boolean]       = None,
  blocks:             Option[Blocks]        = None,
  rights:             Option[Rights]        = None,
  crossword:          Option[Crossword]     = None,
  atoms:              Option[Atoms]         = None,
  stats:              Option[ContentStats]  = None,
  section:            Option[Section]       = None,
  debug:              Option[Debug]         = None,
  isGone:             Option[Boolean]       = None,
  isHosted:           Boolean               = `false`) extends TStruct

object Content extends TStructCodec[Content] {
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
  val w22 = Witness(32)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, ContentType]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  implicit val r5 = new TFieldCodec[w5.T, Option[CapiDateTime]]
  implicit val r6 = new TFieldCodec[w6.T, String]
  implicit val r7 = new TFieldCodec[w7.T, String]
  implicit val r8 = new TFieldCodec[w8.T, String]
  implicit val r9 = new TFieldCodec[w9.T, Option[ContentFields]]
  implicit val r10 = new TFieldCodec[w10.T, List[Tag]]
  implicit val r11 = new TFieldCodec[w11.T, Option[List[Element]]]
  implicit val r12 = new TFieldCodec[w12.T, List[Reference]]
  implicit val r13 = new TFieldCodec[w13.T, Option[Boolean]]
  implicit val r14 = new TFieldCodec[w14.T, Option[Blocks]]
  implicit val r15 = new TFieldCodec[w15.T, Option[Rights]]
  implicit val r16 = new TFieldCodec[w16.T, Option[Crossword]]
  implicit val r17 = new TFieldCodec[w17.T, Option[Atoms]]
  implicit val r18 = new TFieldCodec[w18.T, Option[ContentStats]]
  implicit val r19 = new TFieldCodec[w19.T, Option[Section]]
  implicit val r20 = new TFieldCodec[w20.T, Option[Debug]]
  implicit val r21 = new TFieldCodec[w21.T, Option[Boolean]]
  implicit val r22 = new TFieldCodec[w22.T, Boolean]
  override val defaults = HMap[TFieldCodec](
    w2.value -> ContentType.ARTICLE,
    w3.value -> None,
    w4.value -> None,
    w5.value -> None,
    w9.value -> None,
    w10.value -> List,
    w11.value -> None,
    w12.value -> List,
    w13.value -> None,
    w14.value -> None,
    w15.value -> None,
    w16.value -> None,
    w17.value -> None,
    w18.value -> None,
    w19.value -> None,
    w20.value -> None,
    w21.value -> None,
    w22.value -> `false`)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.id,
    w2.value -> x.`type`,
    w3.value -> x.sectionId,
    w4.value -> x.sectionName,
    w5.value -> x.webPublicationDate,
    w6.value -> x.webTitle,
    w7.value -> x.webUrl,
    w8.value -> x.apiUrl,
    w9.value -> x.fields,
    w10.value -> x.tags,
    w11.value -> x.elements,
    w12.value -> x.references,
    w13.value -> x.isExpired,
    w14.value -> x.blocks,
    w15.value -> x.rights,
    w16.value -> x.crossword,
    w17.value -> x.atoms,
    w18.value -> x.stats,
    w19.value -> x.section,
    w20.value -> x.debug,
    w21.value -> x.isGone,
    w22.value -> x.isHosted)
  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))
    `type` ← m.get(w2.value).orElse(defaults.get(w2.value))
    sectionId ← m.get(w3.value).orElse(defaults.get(w3.value))
    sectionName ← m.get(w4.value).orElse(defaults.get(w4.value))
    webPublicationDate ← m.get(w5.value).orElse(defaults.get(w5.value))
    webTitle ← m.get(w6.value).orElse(defaults.get(w6.value))
    webUrl ← m.get(w7.value).orElse(defaults.get(w7.value))
    apiUrl ← m.get(w8.value).orElse(defaults.get(w8.value))
    fields ← m.get(w9.value).orElse(defaults.get(w9.value))
    tags ← m.get(w10.value).orElse(defaults.get(w10.value))
    elements ← m.get(w11.value).orElse(defaults.get(w11.value))
    references ← m.get(w12.value).orElse(defaults.get(w12.value))
    isExpired ← m.get(w13.value).orElse(defaults.get(w13.value))
    blocks ← m.get(w14.value).orElse(defaults.get(w14.value))
    rights ← m.get(w15.value).orElse(defaults.get(w15.value))
    crossword ← m.get(w16.value).orElse(defaults.get(w16.value))
    atoms ← m.get(w17.value).orElse(defaults.get(w17.value))
    stats ← m.get(w18.value).orElse(defaults.get(w18.value))
    section ← m.get(w19.value).orElse(defaults.get(w19.value))
    debug ← m.get(w20.value).orElse(defaults.get(w20.value))
    isGone ← m.get(w21.value).orElse(defaults.get(w21.value))
    isHosted ← m.get(w22.value).orElse(defaults.get(w22.value))
  } yield Content(
    id,
    `type`,
    sectionId,
    sectionName,
    webPublicationDate,
    webTitle,
    webUrl,
    apiUrl,
    fields,
    tags,
    elements,
    references,
    isExpired,
    blocks,
    rights,
    crossword,
    atoms,
    stats,
    section,
    debug,
    isGone,
    isHosted)
}