package com.gu.contentapi.client.model.v1

case class ContentFields(headline: Option[String] = None, standfirst: Option[String] = None, trailText: Option[String] = None, byline: Option[String] = None, main: Option[String] = None, body: Option[String] = None, newspaperPageNumber: Option[Int] = None, starRating: Option[Int] = None, contributorBio: Option[String] = None, membershipAccess: Option[MembershipTier] = None, wordcount: Option[Int] = None, commentCloseDate: Option[CapiDateTime] = None, commentable: Option[Boolean] = None, creationDate: Option[CapiDateTime] = None, displayHint: Option[String] = None, firstPublicationDate: Option[CapiDateTime] = None, hasStoryPackage: Option[Boolean] = None, internalComposerCode: Option[String] = None, internalOctopusCode: Option[String] = None, internalPageCode: Option[Int] = None, internalStoryPackageCode: Option[Int] = None, isInappropriateForSponsorship: Option[Boolean] = None, isPremoderated: Option[Boolean] = None, lastModified: Option[CapiDateTime] = None, liveBloggingNow: Option[Boolean] = None, newspaperEditionDate: Option[CapiDateTime] = None, productionOffice: Option[Office] = None, publication: Option[String] = None, scheduledPublicationDate: Option[CapiDateTime] = None, secureThumbnail: Option[String] = None, shortUrl: Option[String] = None, shouldHideAdverts: Option[Boolean] = None, showInRelatedContent: Option[Boolean] = None, thumbnail: Option[String] = None, legallySensitive: Option[Boolean] = None, allowUgc: Option[Boolean] = None, sensitive: Option[Boolean] = None, lang: Option[String] = None, internalRevision: Option[Int] = None, internalContentCode: Option[Int] = None, isLive: Option[Boolean] = None, internalShortId: Option[String] = None, shortSocialShareText: Option[String] = None, socialShareText: Option[String] = None, bodyText: Option[String] = None, charCount: Option[Int] = None, internalVideoCode: Option[String] = None, shouldHideReaderRevenue: Option[Boolean] = None) extends TStruct

object ContentFields extends TStructCodec[ContentFields] {
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

  val w34 = Witness(43)

  val w35 = Witness(53)

  val w36 = Witness(63)

  val w37 = Witness(73)

  val w38 = Witness(83)

  val w39 = Witness(93)

  val w40 = Witness(4)

  val w41 = Witness(14)

  val w42 = Witness(24)

  val w43 = Witness(34)

  val w44 = Witness(44)

  val w45 = Witness(54)

  val w46 = Witness(64)

  val w47 = Witness(74)

  val w48 = Witness(84)

  implicit val r1 = new TFieldCodec[w1.T, Option[String]]()

  implicit val r2 = new TFieldCodec[w2.T, Option[String]]()

  implicit val r3 = new TFieldCodec[w3.T, Option[String]]()

  implicit val r4 = new TFieldCodec[w4.T, Option[String]]()

  implicit val r5 = new TFieldCodec[w5.T, Option[String]]()

  implicit val r6 = new TFieldCodec[w6.T, Option[String]]()

  implicit val r7 = new TFieldCodec[w7.T, Option[Int]]()

  implicit val r8 = new TFieldCodec[w8.T, Option[Int]]()

  implicit val r9 = new TFieldCodec[w9.T, Option[String]]()

  implicit val r10 = new TFieldCodec[w10.T, Option[MembershipTier]]()

  implicit val r11 = new TFieldCodec[w11.T, Option[Int]]()

  implicit val r12 = new TFieldCodec[w12.T, Option[CapiDateTime]]()

  implicit val r13 = new TFieldCodec[w13.T, Option[Boolean]]()

  implicit val r14 = new TFieldCodec[w14.T, Option[CapiDateTime]]()

  implicit val r15 = new TFieldCodec[w15.T, Option[String]]()

  implicit val r16 = new TFieldCodec[w16.T, Option[CapiDateTime]]()

  implicit val r17 = new TFieldCodec[w17.T, Option[Boolean]]()

  implicit val r18 = new TFieldCodec[w18.T, Option[String]]()

  implicit val r19 = new TFieldCodec[w19.T, Option[String]]()

  implicit val r20 = new TFieldCodec[w20.T, Option[Int]]()

  implicit val r21 = new TFieldCodec[w21.T, Option[Int]]()

  implicit val r22 = new TFieldCodec[w22.T, Option[Boolean]]()

  implicit val r23 = new TFieldCodec[w23.T, Option[Boolean]]()

  implicit val r24 = new TFieldCodec[w24.T, Option[CapiDateTime]]()

  implicit val r25 = new TFieldCodec[w25.T, Option[Boolean]]()

  implicit val r26 = new TFieldCodec[w26.T, Option[CapiDateTime]]()

  implicit val r27 = new TFieldCodec[w27.T, Option[Office]]()

  implicit val r28 = new TFieldCodec[w28.T, Option[String]]()

  implicit val r29 = new TFieldCodec[w29.T, Option[CapiDateTime]]()

  implicit val r30 = new TFieldCodec[w30.T, Option[String]]()

  implicit val r31 = new TFieldCodec[w31.T, Option[String]]()

  implicit val r32 = new TFieldCodec[w32.T, Option[Boolean]]()

  implicit val r33 = new TFieldCodec[w33.T, Option[Boolean]]()

  implicit val r34 = new TFieldCodec[w34.T, Option[String]]()

  implicit val r35 = new TFieldCodec[w35.T, Option[Boolean]]()

  implicit val r36 = new TFieldCodec[w36.T, Option[Boolean]]()

  implicit val r37 = new TFieldCodec[w37.T, Option[Boolean]]()

  implicit val r38 = new TFieldCodec[w38.T, Option[String]]()

  implicit val r39 = new TFieldCodec[w39.T, Option[Int]]()

  implicit val r40 = new TFieldCodec[w40.T, Option[Int]]()

  implicit val r41 = new TFieldCodec[w41.T, Option[Boolean]]()

  implicit val r42 = new TFieldCodec[w42.T, Option[String]]()

  implicit val r43 = new TFieldCodec[w43.T, Option[String]]()

  implicit val r44 = new TFieldCodec[w44.T, Option[String]]()

  implicit val r45 = new TFieldCodec[w45.T, Option[String]]()

  implicit val r46 = new TFieldCodec[w46.T, Option[Int]]()

  implicit val r47 = new TFieldCodec[w47.T, Option[String]]()

  implicit val r48 = new TFieldCodec[w48.T, Option[Boolean]]()

  override val defaults = HMap[TFieldCodec](w1.value -> None, w2.value -> None, w3.value -> None, w4.value -> None, w5.value -> None, w6.value -> None, w7.value -> None, w8.value -> None, w9.value -> None, w10.value -> None, w11.value -> None, w12.value -> None, w13.value -> None, w14.value -> None, w15.value -> None, w16.value -> None, w17.value -> None, w18.value -> None, w19.value -> None, w20.value -> None, w21.value -> None, w22.value -> None, w23.value -> None, w24.value -> None, w25.value -> None, w26.value -> None, w27.value -> None, w28.value -> None, w29.value -> None, w30.value -> None, w31.value -> None, w32.value -> None, w33.value -> None, w34.value -> None, w35.value -> None, w36.value -> None, w37.value -> None, w38.value -> None, w39.value -> None, w40.value -> None, w41.value -> None, w42.value -> None, w43.value -> None, w44.value -> None, w45.value -> None, w46.value -> None, w47.value -> None, w48.value -> None)

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.headline, w2.value -> x.standfirst, w3.value -> x.trailText, w4.value -> x.byline, w5.value -> x.main, w6.value -> x.body, w7.value -> x.newspaperPageNumber, w8.value -> x.starRating, w9.value -> x.contributorBio, w10.value -> x.membershipAccess, w11.value -> x.wordcount, w12.value -> x.commentCloseDate, w13.value -> x.commentable, w14.value -> x.creationDate, w15.value -> x.displayHint, w16.value -> x.firstPublicationDate, w17.value -> x.hasStoryPackage, w18.value -> x.internalComposerCode, w19.value -> x.internalOctopusCode, w20.value -> x.internalPageCode, w21.value -> x.internalStoryPackageCode, w22.value -> x.isInappropriateForSponsorship, w23.value -> x.isPremoderated, w24.value -> x.lastModified, w25.value -> x.liveBloggingNow, w26.value -> x.newspaperEditionDate, w27.value -> x.productionOffice, w28.value -> x.publication, w29.value -> x.scheduledPublicationDate, w30.value -> x.secureThumbnail, w31.value -> x.shortUrl, w32.value -> x.shouldHideAdverts, w33.value -> x.showInRelatedContent, w34.value -> x.thumbnail, w35.value -> x.legallySensitive, w36.value -> x.allowUgc, w37.value -> x.sensitive, w38.value -> x.lang, w39.value -> x.internalRevision, w40.value -> x.internalContentCode, w41.value -> x.isLive, w42.value -> x.internalShortId, w43.value -> x.shortSocialShareText, w44.value -> x.socialShareText, w45.value -> x.bodyText, w46.value -> x.charCount, w47.value -> x.internalVideoCode, w48.value -> x.shouldHideReaderRevenue)

  override def decode = (m) ⇒ for {
    headline ← m.get(w1.value).orElse(defaults.get(w1.value))

    standfirst ← m.get(w2.value).orElse(defaults.get(w2.value))

    trailText ← m.get(w3.value).orElse(defaults.get(w3.value))

    byline ← m.get(w4.value).orElse(defaults.get(w4.value))

    main ← m.get(w5.value).orElse(defaults.get(w5.value))

    body ← m.get(w6.value).orElse(defaults.get(w6.value))

    newspaperPageNumber ← m.get(w7.value).orElse(defaults.get(w7.value))

    starRating ← m.get(w8.value).orElse(defaults.get(w8.value))

    contributorBio ← m.get(w9.value).orElse(defaults.get(w9.value))

    membershipAccess ← m.get(w10.value).orElse(defaults.get(w10.value))

    wordcount ← m.get(w11.value).orElse(defaults.get(w11.value))

    commentCloseDate ← m.get(w12.value).orElse(defaults.get(w12.value))

    commentable ← m.get(w13.value).orElse(defaults.get(w13.value))

    creationDate ← m.get(w14.value).orElse(defaults.get(w14.value))

    displayHint ← m.get(w15.value).orElse(defaults.get(w15.value))

    firstPublicationDate ← m.get(w16.value).orElse(defaults.get(w16.value))

    hasStoryPackage ← m.get(w17.value).orElse(defaults.get(w17.value))

    internalComposerCode ← m.get(w18.value).orElse(defaults.get(w18.value))

    internalOctopusCode ← m.get(w19.value).orElse(defaults.get(w19.value))

    internalPageCode ← m.get(w20.value).orElse(defaults.get(w20.value))

    internalStoryPackageCode ← m.get(w21.value).orElse(defaults.get(w21.value))

    isInappropriateForSponsorship ← m.get(w22.value).orElse(defaults.get(w22.value))

    isPremoderated ← m.get(w23.value).orElse(defaults.get(w23.value))

    lastModified ← m.get(w24.value).orElse(defaults.get(w24.value))

    liveBloggingNow ← m.get(w25.value).orElse(defaults.get(w25.value))

    newspaperEditionDate ← m.get(w26.value).orElse(defaults.get(w26.value))

    productionOffice ← m.get(w27.value).orElse(defaults.get(w27.value))

    publication ← m.get(w28.value).orElse(defaults.get(w28.value))

    scheduledPublicationDate ← m.get(w29.value).orElse(defaults.get(w29.value))

    secureThumbnail ← m.get(w30.value).orElse(defaults.get(w30.value))

    shortUrl ← m.get(w31.value).orElse(defaults.get(w31.value))

    shouldHideAdverts ← m.get(w32.value).orElse(defaults.get(w32.value))

    showInRelatedContent ← m.get(w33.value).orElse(defaults.get(w33.value))

    thumbnail ← m.get(w34.value).orElse(defaults.get(w34.value))

    legallySensitive ← m.get(w35.value).orElse(defaults.get(w35.value))

    allowUgc ← m.get(w36.value).orElse(defaults.get(w36.value))

    sensitive ← m.get(w37.value).orElse(defaults.get(w37.value))

    lang ← m.get(w38.value).orElse(defaults.get(w38.value))

    internalRevision ← m.get(w39.value).orElse(defaults.get(w39.value))

    internalContentCode ← m.get(w40.value).orElse(defaults.get(w40.value))

    isLive ← m.get(w41.value).orElse(defaults.get(w41.value))

    internalShortId ← m.get(w42.value).orElse(defaults.get(w42.value))

    shortSocialShareText ← m.get(w43.value).orElse(defaults.get(w43.value))

    socialShareText ← m.get(w44.value).orElse(defaults.get(w44.value))

    bodyText ← m.get(w45.value).orElse(defaults.get(w45.value))

    charCount ← m.get(w46.value).orElse(defaults.get(w46.value))

    internalVideoCode ← m.get(w47.value).orElse(defaults.get(w47.value))

    shouldHideReaderRevenue ← m.get(w48.value).orElse(defaults.get(w48.value))
  } yield ContentFields(headline, standfirst, trailText, byline, main, body, newspaperPageNumber, starRating, contributorBio, membershipAccess, wordcount, commentCloseDate, commentable, creationDate, displayHint, firstPublicationDate, hasStoryPackage, internalComposerCode, internalOctopusCode, internalPageCode, internalStoryPackageCode, isInappropriateForSponsorship, isPremoderated, lastModified, liveBloggingNow, newspaperEditionDate, productionOffice, publication, scheduledPublicationDate, secureThumbnail, shortUrl, shouldHideAdverts, showInRelatedContent, thumbnail, legallySensitive, allowUgc, sensitive, lang, internalRevision, internalContentCode, isLive, internalShortId, shortSocialShareText, socialShareText, bodyText, charCount, internalVideoCode, shouldHideReaderRevenue)
}