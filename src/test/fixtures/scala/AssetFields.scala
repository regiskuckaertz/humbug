package com.gu.contentapi.client.model.v1

case class AssetFields(
  aspectRatio:               Option[String]       = None,
  altText:                   Option[String]       = None,
  isInappropriateForAdverts: Option[Boolean]      = None,
  caption:                   Option[String]       = None,
  credit:                    Option[String]       = None,
  embeddable:                Option[Boolean]      = None,
  photographer:              Option[String]       = None,
  source:                    Option[String]       = None,
  stillImageUrl:             Option[String]       = None,
  width:                     Option[Int]          = None,
  height:                    Option[Int]          = None,
  name:                      Option[String]       = None,
  secureFile:                Option[String]       = None,
  isMaster:                  Option[Boolean]      = None,
  sizeInBytes:               Option[Long]         = None,
  durationMinutes:           Option[Int]          = None,
  durationSeconds:           Option[Int]          = None,
  displayCredit:             Option[Boolean]      = None,
  thumbnailUrl:              Option[String]       = None,
  role:                      Option[String]       = None,
  mediaId:                   Option[String]       = None,
  iframeUrl:                 Option[String]       = None,
  scriptName:                Option[String]       = None,
  scriptUrl:                 Option[String]       = None,
  blockAds:                  Option[Boolean]      = None,
  html:                      Option[String]       = None,
  embedType:                 Option[String]       = None,
  explicit:                  Option[Boolean]      = None,
  clean:                     Option[Boolean]      = None,
  thumbnailImageUrl:         Option[String]       = None,
  linkText:                  Option[String]       = None,
  linkPrefix:                Option[String]       = None,
  shortUrl:                  Option[String]       = None,
  imageType:                 Option[String]       = None,
  suppliersReference:        Option[String]       = None,
  mediaApiUri:               Option[String]       = None,
  copyright:                 Option[String]       = None,
  mimeType:                  Option[String]       = None,
  url:                       Option[String]       = None,
  originalUrl:               Option[String]       = None,
  id:                        Option[String]       = None,
  attribution:               Option[String]       = None,
  description:               Option[String]       = None,
  title:                     Option[String]       = None,
  contentAuthSystem:         Option[String]       = None,
  alt:                       Option[String]       = None,
  picdarUrn:                 Option[String]       = None,
  comment:                   Option[String]       = None,
  witnessEmbedType:          Option[String]       = None,
  authorName:                Option[String]       = None,
  authorUsername:            Option[String]       = None,
  authorWitnessProfileUrl:   Option[String]       = None,
  authorGuardianProfileUrl:  Option[String]       = None,
  apiUrl:                    Option[String]       = None,
  dateCreated:               Option[CapiDateTime] = None,
  youtubeUrl:                Option[String]       = None,
  youtubeSource:             Option[String]       = None,
  youtubeTitle:              Option[String]       = None,
  youtubeDescription:        Option[String]       = None,
  youtubeAuthorName:         Option[String]       = None,
  youtubeHtml:               Option[String]       = None,
  venue:                     Option[String]       = None,
  location:                  Option[String]       = None,
  identifier:                Option[String]       = None,
  price:                     Option[String]       = None,
  start:                     Option[CapiDateTime] = None,
  end:                       Option[CapiDateTime] = None,
  safeEmbedCode:             Option[Boolean]      = None) extends TStruct

object AssetFields extends TStructCodec[AssetFields] {
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
  val w49 = Witness(94)
  val w50 = Witness(5)
  val w51 = Witness(15)
  val w52 = Witness(25)
  val w53 = Witness(35)
  val w54 = Witness(45)
  val w55 = Witness(55)
  val w56 = Witness(65)
  val w57 = Witness(75)
  val w58 = Witness(85)
  val w59 = Witness(95)
  val w60 = Witness(6)
  val w61 = Witness(16)
  val w62 = Witness(26)
  val w63 = Witness(36)
  val w64 = Witness(46)
  val w65 = Witness(56)
  val w66 = Witness(66)
  val w67 = Witness(76)
  val w68 = Witness(86)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, Option[Boolean]]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  implicit val r5 = new TFieldCodec[w5.T, Option[String]]
  implicit val r6 = new TFieldCodec[w6.T, Option[Boolean]]
  implicit val r7 = new TFieldCodec[w7.T, Option[String]]
  implicit val r8 = new TFieldCodec[w8.T, Option[String]]
  implicit val r9 = new TFieldCodec[w9.T, Option[String]]
  implicit val r10 = new TFieldCodec[w10.T, Option[Int]]
  implicit val r11 = new TFieldCodec[w11.T, Option[Int]]
  implicit val r12 = new TFieldCodec[w12.T, Option[String]]
  implicit val r13 = new TFieldCodec[w13.T, Option[String]]
  implicit val r14 = new TFieldCodec[w14.T, Option[Boolean]]
  implicit val r15 = new TFieldCodec[w15.T, Option[Long]]
  implicit val r16 = new TFieldCodec[w16.T, Option[Int]]
  implicit val r17 = new TFieldCodec[w17.T, Option[Int]]
  implicit val r18 = new TFieldCodec[w18.T, Option[Boolean]]
  implicit val r19 = new TFieldCodec[w19.T, Option[String]]
  implicit val r20 = new TFieldCodec[w20.T, Option[String]]
  implicit val r21 = new TFieldCodec[w21.T, Option[String]]
  implicit val r22 = new TFieldCodec[w22.T, Option[String]]
  implicit val r23 = new TFieldCodec[w23.T, Option[String]]
  implicit val r24 = new TFieldCodec[w24.T, Option[String]]
  implicit val r25 = new TFieldCodec[w25.T, Option[Boolean]]
  implicit val r26 = new TFieldCodec[w26.T, Option[String]]
  implicit val r27 = new TFieldCodec[w27.T, Option[String]]
  implicit val r28 = new TFieldCodec[w28.T, Option[Boolean]]
  implicit val r29 = new TFieldCodec[w29.T, Option[Boolean]]
  implicit val r30 = new TFieldCodec[w30.T, Option[String]]
  implicit val r31 = new TFieldCodec[w31.T, Option[String]]
  implicit val r32 = new TFieldCodec[w32.T, Option[String]]
  implicit val r33 = new TFieldCodec[w33.T, Option[String]]
  implicit val r34 = new TFieldCodec[w34.T, Option[String]]
  implicit val r35 = new TFieldCodec[w35.T, Option[String]]
  implicit val r36 = new TFieldCodec[w36.T, Option[String]]
  implicit val r37 = new TFieldCodec[w37.T, Option[String]]
  implicit val r38 = new TFieldCodec[w38.T, Option[String]]
  implicit val r39 = new TFieldCodec[w39.T, Option[String]]
  implicit val r40 = new TFieldCodec[w40.T, Option[String]]
  implicit val r41 = new TFieldCodec[w41.T, Option[String]]
  implicit val r42 = new TFieldCodec[w42.T, Option[String]]
  implicit val r43 = new TFieldCodec[w43.T, Option[String]]
  implicit val r44 = new TFieldCodec[w44.T, Option[String]]
  implicit val r45 = new TFieldCodec[w45.T, Option[String]]
  implicit val r46 = new TFieldCodec[w46.T, Option[String]]
  implicit val r47 = new TFieldCodec[w47.T, Option[String]]
  implicit val r48 = new TFieldCodec[w48.T, Option[String]]
  implicit val r49 = new TFieldCodec[w49.T, Option[String]]
  implicit val r50 = new TFieldCodec[w50.T, Option[String]]
  implicit val r51 = new TFieldCodec[w51.T, Option[String]]
  implicit val r52 = new TFieldCodec[w52.T, Option[String]]
  implicit val r53 = new TFieldCodec[w53.T, Option[String]]
  implicit val r54 = new TFieldCodec[w54.T, Option[String]]
  implicit val r55 = new TFieldCodec[w55.T, Option[CapiDateTime]]
  implicit val r56 = new TFieldCodec[w56.T, Option[String]]
  implicit val r57 = new TFieldCodec[w57.T, Option[String]]
  implicit val r58 = new TFieldCodec[w58.T, Option[String]]
  implicit val r59 = new TFieldCodec[w59.T, Option[String]]
  implicit val r60 = new TFieldCodec[w60.T, Option[String]]
  implicit val r61 = new TFieldCodec[w61.T, Option[String]]
  implicit val r62 = new TFieldCodec[w62.T, Option[String]]
  implicit val r63 = new TFieldCodec[w63.T, Option[String]]
  implicit val r64 = new TFieldCodec[w64.T, Option[String]]
  implicit val r65 = new TFieldCodec[w65.T, Option[String]]
  implicit val r66 = new TFieldCodec[w66.T, Option[CapiDateTime]]
  implicit val r67 = new TFieldCodec[w67.T, Option[CapiDateTime]]
  implicit val r68 = new TFieldCodec[w68.T, Option[Boolean]]
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
    w26.value -> None,
    w27.value -> None,
    w28.value -> None,
    w29.value -> None,
    w30.value -> None,
    w31.value -> None,
    w32.value -> None,
    w33.value -> None,
    w34.value -> None,
    w35.value -> None,
    w36.value -> None,
    w37.value -> None,
    w38.value -> None,
    w39.value -> None,
    w40.value -> None,
    w41.value -> None,
    w42.value -> None,
    w43.value -> None,
    w44.value -> None,
    w45.value -> None,
    w46.value -> None,
    w47.value -> None,
    w48.value -> None,
    w49.value -> None,
    w50.value -> None,
    w51.value -> None,
    w52.value -> None,
    w53.value -> None,
    w54.value -> None,
    w55.value -> None,
    w56.value -> None,
    w57.value -> None,
    w58.value -> None,
    w59.value -> None,
    w60.value -> None,
    w61.value -> None,
    w62.value -> None,
    w63.value -> None,
    w64.value -> None,
    w65.value -> None,
    w66.value -> None,
    w67.value -> None,
    w68.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.aspectRatio,
    w2.value -> x.altText,
    w3.value -> x.isInappropriateForAdverts,
    w4.value -> x.caption,
    w5.value -> x.credit,
    w6.value -> x.embeddable,
    w7.value -> x.photographer,
    w8.value -> x.source,
    w9.value -> x.stillImageUrl,
    w10.value -> x.width,
    w11.value -> x.height,
    w12.value -> x.name,
    w13.value -> x.secureFile,
    w14.value -> x.isMaster,
    w15.value -> x.sizeInBytes,
    w16.value -> x.durationMinutes,
    w17.value -> x.durationSeconds,
    w18.value -> x.displayCredit,
    w19.value -> x.thumbnailUrl,
    w20.value -> x.role,
    w21.value -> x.mediaId,
    w22.value -> x.iframeUrl,
    w23.value -> x.scriptName,
    w24.value -> x.scriptUrl,
    w25.value -> x.blockAds,
    w26.value -> x.html,
    w27.value -> x.embedType,
    w28.value -> x.explicit,
    w29.value -> x.clean,
    w30.value -> x.thumbnailImageUrl,
    w31.value -> x.linkText,
    w32.value -> x.linkPrefix,
    w33.value -> x.shortUrl,
    w34.value -> x.imageType,
    w35.value -> x.suppliersReference,
    w36.value -> x.mediaApiUri,
    w37.value -> x.copyright,
    w38.value -> x.mimeType,
    w39.value -> x.url,
    w40.value -> x.originalUrl,
    w41.value -> x.id,
    w42.value -> x.attribution,
    w43.value -> x.description,
    w44.value -> x.title,
    w45.value -> x.contentAuthSystem,
    w46.value -> x.alt,
    w47.value -> x.picdarUrn,
    w48.value -> x.comment,
    w49.value -> x.witnessEmbedType,
    w50.value -> x.authorName,
    w51.value -> x.authorUsername,
    w52.value -> x.authorWitnessProfileUrl,
    w53.value -> x.authorGuardianProfileUrl,
    w54.value -> x.apiUrl,
    w55.value -> x.dateCreated,
    w56.value -> x.youtubeUrl,
    w57.value -> x.youtubeSource,
    w58.value -> x.youtubeTitle,
    w59.value -> x.youtubeDescription,
    w60.value -> x.youtubeAuthorName,
    w61.value -> x.youtubeHtml,
    w62.value -> x.venue,
    w63.value -> x.location,
    w64.value -> x.identifier,
    w65.value -> x.price,
    w66.value -> x.start,
    w67.value -> x.end,
    w68.value -> x.safeEmbedCode)
  override def decode = (m) ⇒ for {
    aspectRatio ← m.get(w1.value).orElse(defaults.get(w1.value))
    altText ← m.get(w2.value).orElse(defaults.get(w2.value))
    isInappropriateForAdverts ← m.get(w3.value).orElse(defaults.get(w3.value))
    caption ← m.get(w4.value).orElse(defaults.get(w4.value))
    credit ← m.get(w5.value).orElse(defaults.get(w5.value))
    embeddable ← m.get(w6.value).orElse(defaults.get(w6.value))
    photographer ← m.get(w7.value).orElse(defaults.get(w7.value))
    source ← m.get(w8.value).orElse(defaults.get(w8.value))
    stillImageUrl ← m.get(w9.value).orElse(defaults.get(w9.value))
    width ← m.get(w10.value).orElse(defaults.get(w10.value))
    height ← m.get(w11.value).orElse(defaults.get(w11.value))
    name ← m.get(w12.value).orElse(defaults.get(w12.value))
    secureFile ← m.get(w13.value).orElse(defaults.get(w13.value))
    isMaster ← m.get(w14.value).orElse(defaults.get(w14.value))
    sizeInBytes ← m.get(w15.value).orElse(defaults.get(w15.value))
    durationMinutes ← m.get(w16.value).orElse(defaults.get(w16.value))
    durationSeconds ← m.get(w17.value).orElse(defaults.get(w17.value))
    displayCredit ← m.get(w18.value).orElse(defaults.get(w18.value))
    thumbnailUrl ← m.get(w19.value).orElse(defaults.get(w19.value))
    role ← m.get(w20.value).orElse(defaults.get(w20.value))
    mediaId ← m.get(w21.value).orElse(defaults.get(w21.value))
    iframeUrl ← m.get(w22.value).orElse(defaults.get(w22.value))
    scriptName ← m.get(w23.value).orElse(defaults.get(w23.value))
    scriptUrl ← m.get(w24.value).orElse(defaults.get(w24.value))
    blockAds ← m.get(w25.value).orElse(defaults.get(w25.value))
    html ← m.get(w26.value).orElse(defaults.get(w26.value))
    embedType ← m.get(w27.value).orElse(defaults.get(w27.value))
    explicit ← m.get(w28.value).orElse(defaults.get(w28.value))
    clean ← m.get(w29.value).orElse(defaults.get(w29.value))
    thumbnailImageUrl ← m.get(w30.value).orElse(defaults.get(w30.value))
    linkText ← m.get(w31.value).orElse(defaults.get(w31.value))
    linkPrefix ← m.get(w32.value).orElse(defaults.get(w32.value))
    shortUrl ← m.get(w33.value).orElse(defaults.get(w33.value))
    imageType ← m.get(w34.value).orElse(defaults.get(w34.value))
    suppliersReference ← m.get(w35.value).orElse(defaults.get(w35.value))
    mediaApiUri ← m.get(w36.value).orElse(defaults.get(w36.value))
    copyright ← m.get(w37.value).orElse(defaults.get(w37.value))
    mimeType ← m.get(w38.value).orElse(defaults.get(w38.value))
    url ← m.get(w39.value).orElse(defaults.get(w39.value))
    originalUrl ← m.get(w40.value).orElse(defaults.get(w40.value))
    id ← m.get(w41.value).orElse(defaults.get(w41.value))
    attribution ← m.get(w42.value).orElse(defaults.get(w42.value))
    description ← m.get(w43.value).orElse(defaults.get(w43.value))
    title ← m.get(w44.value).orElse(defaults.get(w44.value))
    contentAuthSystem ← m.get(w45.value).orElse(defaults.get(w45.value))
    alt ← m.get(w46.value).orElse(defaults.get(w46.value))
    picdarUrn ← m.get(w47.value).orElse(defaults.get(w47.value))
    comment ← m.get(w48.value).orElse(defaults.get(w48.value))
    witnessEmbedType ← m.get(w49.value).orElse(defaults.get(w49.value))
    authorName ← m.get(w50.value).orElse(defaults.get(w50.value))
    authorUsername ← m.get(w51.value).orElse(defaults.get(w51.value))
    authorWitnessProfileUrl ← m.get(w52.value).orElse(defaults.get(w52.value))
    authorGuardianProfileUrl ← m.get(w53.value).orElse(defaults.get(w53.value))
    apiUrl ← m.get(w54.value).orElse(defaults.get(w54.value))
    dateCreated ← m.get(w55.value).orElse(defaults.get(w55.value))
    youtubeUrl ← m.get(w56.value).orElse(defaults.get(w56.value))
    youtubeSource ← m.get(w57.value).orElse(defaults.get(w57.value))
    youtubeTitle ← m.get(w58.value).orElse(defaults.get(w58.value))
    youtubeDescription ← m.get(w59.value).orElse(defaults.get(w59.value))
    youtubeAuthorName ← m.get(w60.value).orElse(defaults.get(w60.value))
    youtubeHtml ← m.get(w61.value).orElse(defaults.get(w61.value))
    venue ← m.get(w62.value).orElse(defaults.get(w62.value))
    location ← m.get(w63.value).orElse(defaults.get(w63.value))
    identifier ← m.get(w64.value).orElse(defaults.get(w64.value))
    price ← m.get(w65.value).orElse(defaults.get(w65.value))
    start ← m.get(w66.value).orElse(defaults.get(w66.value))
    end ← m.get(w67.value).orElse(defaults.get(w67.value))
    safeEmbedCode ← m.get(w68.value).orElse(defaults.get(w68.value))
  } yield AssetFields(
    aspectRatio,
    altText,
    isInappropriateForAdverts,
    caption,
    credit,
    embeddable,
    photographer,
    source,
    stillImageUrl,
    width,
    height,
    name,
    secureFile,
    isMaster,
    sizeInBytes,
    durationMinutes,
    durationSeconds,
    displayCredit,
    thumbnailUrl,
    role,
    mediaId,
    iframeUrl,
    scriptName,
    scriptUrl,
    blockAds,
    html,
    embedType,
    explicit,
    clean,
    thumbnailImageUrl,
    linkText,
    linkPrefix,
    shortUrl,
    imageType,
    suppliersReference,
    mediaApiUri,
    copyright,
    mimeType,
    url,
    originalUrl,
    id,
    attribution,
    description,
    title,
    contentAuthSystem,
    alt,
    picdarUrn,
    comment,
    witnessEmbedType,
    authorName,
    authorUsername,
    authorWitnessProfileUrl,
    authorGuardianProfileUrl,
    apiUrl,
    dateCreated,
    youtubeUrl,
    youtubeSource,
    youtubeTitle,
    youtubeDescription,
    youtubeAuthorName,
    youtubeHtml,
    venue,
    location,
    identifier,
    price,
    start,
    end,
    safeEmbedCode)
}