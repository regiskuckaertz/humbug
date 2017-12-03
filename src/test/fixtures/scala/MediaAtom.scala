package contentatom.media

case class MediaAtom(assets: List[Asset],activeVersion: Option[Version]= None,title: String,category: Category,plutoProjectId: Option[String]= None,duration: Option[Long]= None,source: Option[String]= None,posterUrl: Option[String]= None,description: Option[String]= None,metadata: Option[Metadata]= None,posterImage: Option[Image]= None,trailText: Option[String]= None,byline: Option[List[String]]= None,commissioningDesks: Option[List[String]]= None,keywords: Option[List[String]]= None,trailImage: Option[Image]= None,optimisedForWeb: Option[Boolean]= None,commentsEnabled: Option[Boolean]= None,suppressRelatedContent: Option[Boolean]= None) extends TStruct

object MediaAtom extends TStructCodec[MediaAtom]{
val w1 = Witness(2)

val w2 = Witness(3)

val w3 = Witness(4)

val w4 = Witness(5)

val w5 = Witness(6)

val w6 = Witness(7)

val w7 = Witness(8)

val w8 = Witness(9)

val w9 = Witness(1)

val w10 = Witness(11)

val w11 = Witness(21)

val w12 = Witness(41)

val w13 = Witness(51)

val w14 = Witness(61)

val w15 = Witness(71)

val w16 = Witness(81)

val w17 = Witness(91)

val w18 = Witness(2)

val w19 = Witness(12)

implicit val r1 = new TFieldCodec[w1.T,List[Asset]]()

implicit val r2 = new TFieldCodec[w2.T,Option[Version]]()

implicit val r3 = new TFieldCodec[w3.T,String]()

implicit val r4 = new TFieldCodec[w4.T,Category]()

implicit val r5 = new TFieldCodec[w5.T,Option[String]]()

implicit val r6 = new TFieldCodec[w6.T,Option[Long]]()

implicit val r7 = new TFieldCodec[w7.T,Option[String]]()

implicit val r8 = new TFieldCodec[w8.T,Option[String]]()

implicit val r9 = new TFieldCodec[w9.T,Option[String]]()

implicit val r10 = new TFieldCodec[w10.T,Option[Metadata]]()

implicit val r11 = new TFieldCodec[w11.T,Option[Image]]()

implicit val r12 = new TFieldCodec[w12.T,Option[String]]()

implicit val r13 = new TFieldCodec[w13.T,Option[List[String]]]()

implicit val r14 = new TFieldCodec[w14.T,Option[List[String]]]()

implicit val r15 = new TFieldCodec[w15.T,Option[List[String]]]()

implicit val r16 = new TFieldCodec[w16.T,Option[Image]]()

implicit val r17 = new TFieldCodec[w17.T,Option[Boolean]]()

implicit val r18 = new TFieldCodec[w18.T,Option[Boolean]]()

implicit val r19 = new TFieldCodec[w19.T,Option[Boolean]]()

override val defaults = HMap[TFieldCodec](w2.value -> None,w5.value -> None,w6.value -> None,w7.value -> None,w8.value -> None,w9.value -> None,w10.value -> None,w11.value -> None,w12.value -> None,w13.value -> None,w14.value -> None,w15.value -> None,w16.value -> None,w17.value -> None,w18.value -> None,w19.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.assets,w2.value -> x.activeVersion,w3.value -> x.title,w4.value -> x.category,w5.value -> x.plutoProjectId,w6.value -> x.duration,w7.value -> x.source,w8.value -> x.posterUrl,w9.value -> x.description,w10.value -> x.metadata,w11.value -> x.posterImage,w12.value -> x.trailText,w13.value -> x.byline,w14.value -> x.commissioningDesks,w15.value -> x.keywords,w16.value -> x.trailImage,w17.value -> x.optimisedForWeb,w18.value -> x.commentsEnabled,w19.value -> x.suppressRelatedContent)

override def decode = (m) => for {
assets <- m.get(w1.value).orElse(defaults.get(w1.value))

activeVersion <- m.get(w2.value).orElse(defaults.get(w2.value))

title <- m.get(w3.value).orElse(defaults.get(w3.value))

category <- m.get(w4.value).orElse(defaults.get(w4.value))

plutoProjectId <- m.get(w5.value).orElse(defaults.get(w5.value))

duration <- m.get(w6.value).orElse(defaults.get(w6.value))

source <- m.get(w7.value).orElse(defaults.get(w7.value))

posterUrl <- m.get(w8.value).orElse(defaults.get(w8.value))

description <- m.get(w9.value).orElse(defaults.get(w9.value))

metadata <- m.get(w10.value).orElse(defaults.get(w10.value))

posterImage <- m.get(w11.value).orElse(defaults.get(w11.value))

trailText <- m.get(w12.value).orElse(defaults.get(w12.value))

byline <- m.get(w13.value).orElse(defaults.get(w13.value))

commissioningDesks <- m.get(w14.value).orElse(defaults.get(w14.value))

keywords <- m.get(w15.value).orElse(defaults.get(w15.value))

trailImage <- m.get(w16.value).orElse(defaults.get(w16.value))

optimisedForWeb <- m.get(w17.value).orElse(defaults.get(w17.value))

commentsEnabled <- m.get(w18.value).orElse(defaults.get(w18.value))

suppressRelatedContent <- m.get(w19.value).orElse(defaults.get(w19.value))
} yield MediaAtom(assets,activeVersion,title,category,plutoProjectId,duration,source,posterUrl,description,metadata,posterImage,trailText,byline,commissioningDesks,keywords,trailImage,optimisedForWeb,commentsEnabled,suppressRelatedContent)
}