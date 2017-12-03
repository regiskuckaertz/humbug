package com.gu.contentapi.client.model.v1

case class BlockElement(
  `type`:              ElementType,
  assets:              List[Asset],
  textTypeData:        Option[TextElementFields]        = None,
  videoTypeData:       Option[VideoElementFields]       = None,
  tweetTypeData:       Option[TweetElementFields]       = None,
  imageTypeData:       Option[ImageElementFields]       = None,
  audioTypeData:       Option[AudioElementFields]       = None,
  pullquoteTypeData:   Option[PullquoteElementFields]   = None,
  interactiveTypeData: Option[InteractiveElementFields] = None,
  mapTypeData:         Option[StandardElementFields]    = None,
  documentTypeData:    Option[StandardElementFields]    = None,
  tableTypeData:       Option[StandardElementFields]    = None,
  witnessTypeData:     Option[WitnessElementFields]     = None,
  richLinkTypeData:    Option[RichLinkElementFields]    = None,
  membershipTypeData:  Option[MembershipElementFields]  = None,
  embedTypeData:       Option[EmbedElementFields]       = None,
  instagramTypeData:   Option[InstagramElementFields]   = None,
  commentTypeData:     Option[CommentElementFields]     = None,
  vineTypeData:        Option[VineElementFields]        = None,
  contentAtomTypeData: Option[ContentAtomElementFields] = None) extends TStruct

object BlockElement extends TStructCodec[BlockElement] {
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
  implicit val r1 = new TFieldCodec[w1.T, ElementType]
  implicit val r2 = new TFieldCodec[w2.T, List[Asset]]
  implicit val r3 = new TFieldCodec[w3.T, Option[TextElementFields]]
  implicit val r4 = new TFieldCodec[w4.T, Option[VideoElementFields]]
  implicit val r5 = new TFieldCodec[w5.T, Option[TweetElementFields]]
  implicit val r6 = new TFieldCodec[w6.T, Option[ImageElementFields]]
  implicit val r7 = new TFieldCodec[w7.T, Option[AudioElementFields]]
  implicit val r8 = new TFieldCodec[w8.T, Option[PullquoteElementFields]]
  implicit val r9 = new TFieldCodec[w9.T, Option[InteractiveElementFields]]
  implicit val r10 = new TFieldCodec[w10.T, Option[StandardElementFields]]
  implicit val r11 = new TFieldCodec[w11.T, Option[StandardElementFields]]
  implicit val r12 = new TFieldCodec[w12.T, Option[StandardElementFields]]
  implicit val r13 = new TFieldCodec[w13.T, Option[WitnessElementFields]]
  implicit val r14 = new TFieldCodec[w14.T, Option[RichLinkElementFields]]
  implicit val r15 = new TFieldCodec[w15.T, Option[MembershipElementFields]]
  implicit val r16 = new TFieldCodec[w16.T, Option[EmbedElementFields]]
  implicit val r17 = new TFieldCodec[w17.T, Option[InstagramElementFields]]
  implicit val r18 = new TFieldCodec[w18.T, Option[CommentElementFields]]
  implicit val r19 = new TFieldCodec[w19.T, Option[VineElementFields]]
  implicit val r20 = new TFieldCodec[w20.T, Option[ContentAtomElementFields]]
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
    w20.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.`type`,
    w2.value -> x.assets,
    w3.value -> x.textTypeData,
    w4.value -> x.videoTypeData,
    w5.value -> x.tweetTypeData,
    w6.value -> x.imageTypeData,
    w7.value -> x.audioTypeData,
    w8.value -> x.pullquoteTypeData,
    w9.value -> x.interactiveTypeData,
    w10.value -> x.mapTypeData,
    w11.value -> x.documentTypeData,
    w12.value -> x.tableTypeData,
    w13.value -> x.witnessTypeData,
    w14.value -> x.richLinkTypeData,
    w15.value -> x.membershipTypeData,
    w16.value -> x.embedTypeData,
    w17.value -> x.instagramTypeData,
    w18.value -> x.commentTypeData,
    w19.value -> x.vineTypeData,
    w20.value -> x.contentAtomTypeData)
  override def decode = (m) ⇒ for {
    `type` ← m.get(w1.value).orElse(defaults.get(w1.value))
    assets ← m.get(w2.value).orElse(defaults.get(w2.value))
    textTypeData ← m.get(w3.value).orElse(defaults.get(w3.value))
    videoTypeData ← m.get(w4.value).orElse(defaults.get(w4.value))
    tweetTypeData ← m.get(w5.value).orElse(defaults.get(w5.value))
    imageTypeData ← m.get(w6.value).orElse(defaults.get(w6.value))
    audioTypeData ← m.get(w7.value).orElse(defaults.get(w7.value))
    pullquoteTypeData ← m.get(w8.value).orElse(defaults.get(w8.value))
    interactiveTypeData ← m.get(w9.value).orElse(defaults.get(w9.value))
    mapTypeData ← m.get(w10.value).orElse(defaults.get(w10.value))
    documentTypeData ← m.get(w11.value).orElse(defaults.get(w11.value))
    tableTypeData ← m.get(w12.value).orElse(defaults.get(w12.value))
    witnessTypeData ← m.get(w13.value).orElse(defaults.get(w13.value))
    richLinkTypeData ← m.get(w14.value).orElse(defaults.get(w14.value))
    membershipTypeData ← m.get(w15.value).orElse(defaults.get(w15.value))
    embedTypeData ← m.get(w16.value).orElse(defaults.get(w16.value))
    instagramTypeData ← m.get(w17.value).orElse(defaults.get(w17.value))
    commentTypeData ← m.get(w18.value).orElse(defaults.get(w18.value))
    vineTypeData ← m.get(w19.value).orElse(defaults.get(w19.value))
    contentAtomTypeData ← m.get(w20.value).orElse(defaults.get(w20.value))
  } yield BlockElement(
    `type`,
    assets,
    textTypeData,
    videoTypeData,
    tweetTypeData,
    imageTypeData,
    audioTypeData,
    pullquoteTypeData,
    interactiveTypeData,
    mapTypeData,
    documentTypeData,
    tableTypeData,
    witnessTypeData,
    richLinkTypeData,
    membershipTypeData,
    embedTypeData,
    instagramTypeData,
    commentTypeData,
    vineTypeData,
    contentAtomTypeData)
}