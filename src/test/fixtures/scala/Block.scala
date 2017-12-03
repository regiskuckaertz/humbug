package com.gu.contentapi.client.model.v1

case class Block(
  id:                 String,
  bodyHtml:           String,
  bodyTextSummary:    String,
  title:              Option[String]       = None,
  attributes:         BlockAttributes,
  published:          Boolean,
  createdDate:        Option[CapiDateTime] = None,
  firstPublishedDate: Option[CapiDateTime] = None,
  publishedDate:      Option[CapiDateTime] = None,
  lastModifiedDate:   Option[CapiDateTime] = None,
  contributors:       List[String],
  createdBy:          Option[User]         = None,
  lastModifiedBy:     Option[User]         = None,
  elements:           List[BlockElement]   = List) extends TStruct

object Block extends TStructCodec[Block] {
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
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, String]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  implicit val r5 = new TFieldCodec[w5.T, BlockAttributes]
  implicit val r6 = new TFieldCodec[w6.T, Boolean]
  implicit val r7 = new TFieldCodec[w7.T, Option[CapiDateTime]]
  implicit val r8 = new TFieldCodec[w8.T, Option[CapiDateTime]]
  implicit val r9 = new TFieldCodec[w9.T, Option[CapiDateTime]]
  implicit val r10 = new TFieldCodec[w10.T, Option[CapiDateTime]]
  implicit val r11 = new TFieldCodec[w11.T, List[String]]
  implicit val r12 = new TFieldCodec[w12.T, Option[User]]
  implicit val r13 = new TFieldCodec[w13.T, Option[User]]
  implicit val r14 = new TFieldCodec[w14.T, List[BlockElement]]
  override val defaults = HMap[TFieldCodec](
    w4.value -> None,
    w7.value -> None,
    w8.value -> None,
    w9.value -> None,
    w10.value -> None,
    w12.value -> None,
    w13.value -> None,
    w14.value -> List)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.id,
    w2.value -> x.bodyHtml,
    w3.value -> x.bodyTextSummary,
    w4.value -> x.title,
    w5.value -> x.attributes,
    w6.value -> x.published,
    w7.value -> x.createdDate,
    w8.value -> x.firstPublishedDate,
    w9.value -> x.publishedDate,
    w10.value -> x.lastModifiedDate,
    w11.value -> x.contributors,
    w12.value -> x.createdBy,
    w13.value -> x.lastModifiedBy,
    w14.value -> x.elements)
  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))
    bodyHtml ← m.get(w2.value).orElse(defaults.get(w2.value))
    bodyTextSummary ← m.get(w3.value).orElse(defaults.get(w3.value))
    title ← m.get(w4.value).orElse(defaults.get(w4.value))
    attributes ← m.get(w5.value).orElse(defaults.get(w5.value))
    published ← m.get(w6.value).orElse(defaults.get(w6.value))
    createdDate ← m.get(w7.value).orElse(defaults.get(w7.value))
    firstPublishedDate ← m.get(w8.value).orElse(defaults.get(w8.value))
    publishedDate ← m.get(w9.value).orElse(defaults.get(w9.value))
    lastModifiedDate ← m.get(w10.value).orElse(defaults.get(w10.value))
    contributors ← m.get(w11.value).orElse(defaults.get(w11.value))
    createdBy ← m.get(w12.value).orElse(defaults.get(w12.value))
    lastModifiedBy ← m.get(w13.value).orElse(defaults.get(w13.value))
    elements ← m.get(w14.value).orElse(defaults.get(w14.value))
  } yield Block(
    id,
    bodyHtml,
    bodyTextSummary,
    title,
    attributes,
    published,
    createdDate,
    firstPublishedDate,
    publishedDate,
    lastModifiedDate,
    contributors,
    createdBy,
    lastModifiedBy,
    elements)
}