package com.gu.storypackage.model.v1

case class Article(
  id:                  String,
  articleType:         ArticleType,
  group:               Group,
  headline:            Option[String]  = None,
  href:                Option[String]  = None,
  trailText:           Option[String]  = None,
  imageSrc:            Option[String]  = None,
  isBoosted:           Option[Boolean] = None,
  imageHide:           Option[Boolean] = None,
  showMainVideo:       Option[Boolean] = None,
  showKickerTag:       Option[Boolean] = None,
  showKickerSection:   Option[Boolean] = None,
  byline:              Option[String]  = None,
  customKicker:        Option[String]  = None,
  showBoostedHeadline: Option[Boolean] = None,
  showQuotedHeadline:  Option[Boolean] = None) extends TStruct

object Article extends TStructCodec[Article] {
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
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, ArticleType]
  implicit val r3 = new TFieldCodec[w3.T, Group]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  implicit val r5 = new TFieldCodec[w5.T, Option[String]]
  implicit val r6 = new TFieldCodec[w6.T, Option[String]]
  implicit val r7 = new TFieldCodec[w7.T, Option[String]]
  implicit val r8 = new TFieldCodec[w8.T, Option[Boolean]]
  implicit val r9 = new TFieldCodec[w9.T, Option[Boolean]]
  implicit val r10 = new TFieldCodec[w10.T, Option[Boolean]]
  implicit val r11 = new TFieldCodec[w11.T, Option[Boolean]]
  implicit val r12 = new TFieldCodec[w12.T, Option[Boolean]]
  implicit val r13 = new TFieldCodec[w13.T, Option[String]]
  implicit val r14 = new TFieldCodec[w14.T, Option[String]]
  implicit val r15 = new TFieldCodec[w15.T, Option[Boolean]]
  implicit val r16 = new TFieldCodec[w16.T, Option[Boolean]]
  override val defaults = HMap[TFieldCodec](
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
    w16.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.id,
    w2.value -> x.articleType,
    w3.value -> x.group,
    w4.value -> x.headline,
    w5.value -> x.href,
    w6.value -> x.trailText,
    w7.value -> x.imageSrc,
    w8.value -> x.isBoosted,
    w9.value -> x.imageHide,
    w10.value -> x.showMainVideo,
    w11.value -> x.showKickerTag,
    w12.value -> x.showKickerSection,
    w13.value -> x.byline,
    w14.value -> x.customKicker,
    w15.value -> x.showBoostedHeadline,
    w16.value -> x.showQuotedHeadline)
  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))
    articleType ← m.get(w2.value).orElse(defaults.get(w2.value))
    group ← m.get(w3.value).orElse(defaults.get(w3.value))
    headline ← m.get(w4.value).orElse(defaults.get(w4.value))
    href ← m.get(w5.value).orElse(defaults.get(w5.value))
    trailText ← m.get(w6.value).orElse(defaults.get(w6.value))
    imageSrc ← m.get(w7.value).orElse(defaults.get(w7.value))
    isBoosted ← m.get(w8.value).orElse(defaults.get(w8.value))
    imageHide ← m.get(w9.value).orElse(defaults.get(w9.value))
    showMainVideo ← m.get(w10.value).orElse(defaults.get(w10.value))
    showKickerTag ← m.get(w11.value).orElse(defaults.get(w11.value))
    showKickerSection ← m.get(w12.value).orElse(defaults.get(w12.value))
    byline ← m.get(w13.value).orElse(defaults.get(w13.value))
    customKicker ← m.get(w14.value).orElse(defaults.get(w14.value))
    showBoostedHeadline ← m.get(w15.value).orElse(defaults.get(w15.value))
    showQuotedHeadline ← m.get(w16.value).orElse(defaults.get(w16.value))
  } yield Article(
    id,
    articleType,
    group,
    headline,
    href,
    trailText,
    imageSrc,
    isBoosted,
    imageHide,
    showMainVideo,
    showKickerTag,
    showKickerSection,
    byline,
    customKicker,
    showBoostedHeadline,
    showQuotedHeadline)
}