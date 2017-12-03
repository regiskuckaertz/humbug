package com.gu.contentapi.client.model.v1

case class ItemResponse(
  status:         String,
  userTier:       String,
  total:          Option[Int]           = None,
  startIndex:     Option[Int]           = None,
  pageSize:       Option[Int]           = None,
  currentPage:    Option[Int]           = None,
  pages:          Option[Int]           = None,
  orderBy:        Option[String]        = None,
  content:        Option[Content]       = None,
  tag:            Option[Tag]           = None,
  edition:        Option[Edition]       = None,
  section:        Option[Section]       = None,
  results:        Option[List[Content]] = None,
  quiz:           Option[Atom]          = None,
  relatedContent: Option[List[Content]] = None,
  storyPackage:   Option[List[Content]] = None,
  editorsPicks:   Option[List[Content]] = None,
  mostViewed:     Option[List[Content]] = None,
  leadContent:    Option[List[Content]] = None,
  packages:       Option[List[Package]] = None,
  viewpoints:     Option[List[Atom]]    = None,
  media:          Option[Atom]          = None,
  explainer:      Option[Atom]          = None,
  cta:            Option[Atom]          = None,
  interactive:    Option[Atom]          = None,
  review:         Option[Atom]          = None,
  recipe:         Option[Atom]          = None,
  storyquestions: Option[Atom]          = None,
  story:          Option[Story]         = None,
  qanda:          Option[Atom]          = None,
  guide:          Option[Atom]          = None,
  profile:        Option[Atom]          = None,
  timeline:       Option[Atom]          = None) extends TStruct

object ItemResponse extends TStructCodec[ItemResponse] {
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
  val w27 = Witness(72)
  val w28 = Witness(82)
  val w29 = Witness(92)
  val w30 = Witness(3)
  val w31 = Witness(13)
  val w32 = Witness(23)
  val w33 = Witness(33)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, Option[Int]]
  implicit val r4 = new TFieldCodec[w4.T, Option[Int]]
  implicit val r5 = new TFieldCodec[w5.T, Option[Int]]
  implicit val r6 = new TFieldCodec[w6.T, Option[Int]]
  implicit val r7 = new TFieldCodec[w7.T, Option[Int]]
  implicit val r8 = new TFieldCodec[w8.T, Option[String]]
  implicit val r9 = new TFieldCodec[w9.T, Option[Content]]
  implicit val r10 = new TFieldCodec[w10.T, Option[Tag]]
  implicit val r11 = new TFieldCodec[w11.T, Option[Edition]]
  implicit val r12 = new TFieldCodec[w12.T, Option[Section]]
  implicit val r13 = new TFieldCodec[w13.T, Option[List[Content]]]
  implicit val r14 = new TFieldCodec[w14.T, Option[Atom]]
  implicit val r15 = new TFieldCodec[w15.T, Option[List[Content]]]
  implicit val r16 = new TFieldCodec[w16.T, Option[List[Content]]]
  implicit val r17 = new TFieldCodec[w17.T, Option[List[Content]]]
  implicit val r18 = new TFieldCodec[w18.T, Option[List[Content]]]
  implicit val r19 = new TFieldCodec[w19.T, Option[List[Content]]]
  implicit val r20 = new TFieldCodec[w20.T, Option[List[Package]]]
  implicit val r21 = new TFieldCodec[w21.T, Option[List[Atom]]]
  implicit val r22 = new TFieldCodec[w22.T, Option[Atom]]
  implicit val r23 = new TFieldCodec[w23.T, Option[Atom]]
  implicit val r24 = new TFieldCodec[w24.T, Option[Atom]]
  implicit val r25 = new TFieldCodec[w25.T, Option[Atom]]
  implicit val r26 = new TFieldCodec[w26.T, Option[Atom]]
  implicit val r27 = new TFieldCodec[w27.T, Option[Atom]]
  implicit val r28 = new TFieldCodec[w28.T, Option[Atom]]
  implicit val r29 = new TFieldCodec[w29.T, Option[Story]]
  implicit val r30 = new TFieldCodec[w30.T, Option[Atom]]
  implicit val r31 = new TFieldCodec[w31.T, Option[Atom]]
  implicit val r32 = new TFieldCodec[w32.T, Option[Atom]]
  implicit val r33 = new TFieldCodec[w33.T, Option[Atom]]
  override val defaults = HMap[TFieldCodec](
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
    w26.value -> None,
    w27.value -> None,
    w28.value -> None,
    w29.value -> None,
    w30.value -> None,
    w31.value -> None,
    w32.value -> None,
    w33.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.status,
    w2.value -> x.userTier,
    w3.value -> x.total,
    w4.value -> x.startIndex,
    w5.value -> x.pageSize,
    w6.value -> x.currentPage,
    w7.value -> x.pages,
    w8.value -> x.orderBy,
    w9.value -> x.content,
    w10.value -> x.tag,
    w11.value -> x.edition,
    w12.value -> x.section,
    w13.value -> x.results,
    w14.value -> x.quiz,
    w15.value -> x.relatedContent,
    w16.value -> x.storyPackage,
    w17.value -> x.editorsPicks,
    w18.value -> x.mostViewed,
    w19.value -> x.leadContent,
    w20.value -> x.packages,
    w21.value -> x.viewpoints,
    w22.value -> x.media,
    w23.value -> x.explainer,
    w24.value -> x.cta,
    w25.value -> x.interactive,
    w26.value -> x.review,
    w27.value -> x.recipe,
    w28.value -> x.storyquestions,
    w29.value -> x.story,
    w30.value -> x.qanda,
    w31.value -> x.guide,
    w32.value -> x.profile,
    w33.value -> x.timeline)
  override def decode = (m) ⇒ for {
    status ← m.get(w1.value).orElse(defaults.get(w1.value))
    userTier ← m.get(w2.value).orElse(defaults.get(w2.value))
    total ← m.get(w3.value).orElse(defaults.get(w3.value))
    startIndex ← m.get(w4.value).orElse(defaults.get(w4.value))
    pageSize ← m.get(w5.value).orElse(defaults.get(w5.value))
    currentPage ← m.get(w6.value).orElse(defaults.get(w6.value))
    pages ← m.get(w7.value).orElse(defaults.get(w7.value))
    orderBy ← m.get(w8.value).orElse(defaults.get(w8.value))
    content ← m.get(w9.value).orElse(defaults.get(w9.value))
    tag ← m.get(w10.value).orElse(defaults.get(w10.value))
    edition ← m.get(w11.value).orElse(defaults.get(w11.value))
    section ← m.get(w12.value).orElse(defaults.get(w12.value))
    results ← m.get(w13.value).orElse(defaults.get(w13.value))
    quiz ← m.get(w14.value).orElse(defaults.get(w14.value))
    relatedContent ← m.get(w15.value).orElse(defaults.get(w15.value))
    storyPackage ← m.get(w16.value).orElse(defaults.get(w16.value))
    editorsPicks ← m.get(w17.value).orElse(defaults.get(w17.value))
    mostViewed ← m.get(w18.value).orElse(defaults.get(w18.value))
    leadContent ← m.get(w19.value).orElse(defaults.get(w19.value))
    packages ← m.get(w20.value).orElse(defaults.get(w20.value))
    viewpoints ← m.get(w21.value).orElse(defaults.get(w21.value))
    media ← m.get(w22.value).orElse(defaults.get(w22.value))
    explainer ← m.get(w23.value).orElse(defaults.get(w23.value))
    cta ← m.get(w24.value).orElse(defaults.get(w24.value))
    interactive ← m.get(w25.value).orElse(defaults.get(w25.value))
    review ← m.get(w26.value).orElse(defaults.get(w26.value))
    recipe ← m.get(w27.value).orElse(defaults.get(w27.value))
    storyquestions ← m.get(w28.value).orElse(defaults.get(w28.value))
    story ← m.get(w29.value).orElse(defaults.get(w29.value))
    qanda ← m.get(w30.value).orElse(defaults.get(w30.value))
    guide ← m.get(w31.value).orElse(defaults.get(w31.value))
    profile ← m.get(w32.value).orElse(defaults.get(w32.value))
    timeline ← m.get(w33.value).orElse(defaults.get(w33.value))
  } yield ItemResponse(
    status,
    userTier,
    total,
    startIndex,
    pageSize,
    currentPage,
    pages,
    orderBy,
    content,
    tag,
    edition,
    section,
    results,
    quiz,
    relatedContent,
    storyPackage,
    editorsPicks,
    mostViewed,
    leadContent,
    packages,
    viewpoints,
    media,
    explainer,
    cta,
    interactive,
    review,
    recipe,
    storyquestions,
    story,
    qanda,
    guide,
    profile,
    timeline)
}