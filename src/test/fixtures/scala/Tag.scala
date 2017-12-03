package com.gu.contentapi.client.model.v1

case class Tag(id: String,`type`: TagType,sectionId: Option[String]= None,sectionName: Option[String]= None,webTitle: String,webUrl: String,apiUrl: String,references: List[Reference],description: Option[String]= None,bio: Option[String]= None,bylineImageUrl: Option[String]= None,bylineLargeImageUrl: Option[String]= None,podcast: Option[Podcast]= None,firstName: Option[String]= None,lastName: Option[String]= None,emailAddress: Option[String]= None,twitterHandle: Option[String]= None,activeSponsorships: Option[List[Sponsorship]]= None,paidContentType: Option[String]= None,paidContentCampaignColour: Option[String]= None,rcsId: Option[String]= None,r2ContributorId: Option[String]= None,tagCategories: Option[Set[String]]= None,entityIds: Option[Set[String]]= None) extends TStruct

object Tag extends TStructCodec[Tag]{
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

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,TagType]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

implicit val r4 = new TFieldCodec[w4.T,Option[String]]()

implicit val r5 = new TFieldCodec[w5.T,String]()

implicit val r6 = new TFieldCodec[w6.T,String]()

implicit val r7 = new TFieldCodec[w7.T,String]()

implicit val r8 = new TFieldCodec[w8.T,List[Reference]]()

implicit val r9 = new TFieldCodec[w9.T,Option[String]]()

implicit val r10 = new TFieldCodec[w10.T,Option[String]]()

implicit val r11 = new TFieldCodec[w11.T,Option[String]]()

implicit val r12 = new TFieldCodec[w12.T,Option[String]]()

implicit val r13 = new TFieldCodec[w13.T,Option[Podcast]]()

implicit val r14 = new TFieldCodec[w14.T,Option[String]]()

implicit val r15 = new TFieldCodec[w15.T,Option[String]]()

implicit val r16 = new TFieldCodec[w16.T,Option[String]]()

implicit val r17 = new TFieldCodec[w17.T,Option[String]]()

implicit val r18 = new TFieldCodec[w18.T,Option[List[Sponsorship]]]()

implicit val r19 = new TFieldCodec[w19.T,Option[String]]()

implicit val r20 = new TFieldCodec[w20.T,Option[String]]()

implicit val r21 = new TFieldCodec[w21.T,Option[String]]()

implicit val r22 = new TFieldCodec[w22.T,Option[String]]()

implicit val r23 = new TFieldCodec[w23.T,Option[Set[String]]]()

implicit val r24 = new TFieldCodec[w24.T,Option[Set[String]]]()

override val defaults = HMap[TFieldCodec](w3.value -> None,w4.value -> None,w9.value -> None,w10.value -> None,w11.value -> None,w12.value -> None,w13.value -> None,w14.value -> None,w15.value -> None,w16.value -> None,w17.value -> None,w18.value -> None,w19.value -> None,w20.value -> None,w21.value -> None,w22.value -> None,w23.value -> None,w24.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.id,w2.value -> x.`type`,w3.value -> x.sectionId,w4.value -> x.sectionName,w5.value -> x.webTitle,w6.value -> x.webUrl,w7.value -> x.apiUrl,w8.value -> x.references,w9.value -> x.description,w10.value -> x.bio,w11.value -> x.bylineImageUrl,w12.value -> x.bylineLargeImageUrl,w13.value -> x.podcast,w14.value -> x.firstName,w15.value -> x.lastName,w16.value -> x.emailAddress,w17.value -> x.twitterHandle,w18.value -> x.activeSponsorships,w19.value -> x.paidContentType,w20.value -> x.paidContentCampaignColour,w21.value -> x.rcsId,w22.value -> x.r2ContributorId,w23.value -> x.tagCategories,w24.value -> x.entityIds)

override def decode = (m) => for {
id <- m.get(w1.value).orElse(defaults.get(w1.value))

`type` <- m.get(w2.value).orElse(defaults.get(w2.value))

sectionId <- m.get(w3.value).orElse(defaults.get(w3.value))

sectionName <- m.get(w4.value).orElse(defaults.get(w4.value))

webTitle <- m.get(w5.value).orElse(defaults.get(w5.value))

webUrl <- m.get(w6.value).orElse(defaults.get(w6.value))

apiUrl <- m.get(w7.value).orElse(defaults.get(w7.value))

references <- m.get(w8.value).orElse(defaults.get(w8.value))

description <- m.get(w9.value).orElse(defaults.get(w9.value))

bio <- m.get(w10.value).orElse(defaults.get(w10.value))

bylineImageUrl <- m.get(w11.value).orElse(defaults.get(w11.value))

bylineLargeImageUrl <- m.get(w12.value).orElse(defaults.get(w12.value))

podcast <- m.get(w13.value).orElse(defaults.get(w13.value))

firstName <- m.get(w14.value).orElse(defaults.get(w14.value))

lastName <- m.get(w15.value).orElse(defaults.get(w15.value))

emailAddress <- m.get(w16.value).orElse(defaults.get(w16.value))

twitterHandle <- m.get(w17.value).orElse(defaults.get(w17.value))

activeSponsorships <- m.get(w18.value).orElse(defaults.get(w18.value))

paidContentType <- m.get(w19.value).orElse(defaults.get(w19.value))

paidContentCampaignColour <- m.get(w20.value).orElse(defaults.get(w20.value))

rcsId <- m.get(w21.value).orElse(defaults.get(w21.value))

r2ContributorId <- m.get(w22.value).orElse(defaults.get(w22.value))

tagCategories <- m.get(w23.value).orElse(defaults.get(w23.value))

entityIds <- m.get(w24.value).orElse(defaults.get(w24.value))
} yield Tag(id,`type`,sectionId,sectionName,webTitle,webUrl,apiUrl,references,description,bio,bylineImageUrl,bylineLargeImageUrl,podcast,firstName,lastName,emailAddress,twitterHandle,activeSponsorships,paidContentType,paidContentCampaignColour,rcsId,r2ContributorId,tagCategories,entityIds)
}