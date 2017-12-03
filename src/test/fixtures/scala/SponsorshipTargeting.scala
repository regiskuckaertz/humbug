package com.gu.contentapi.client.model.v1

case class SponsorshipTargeting(publishedSince: Option[CapiDateTime]= None,validEditions: Option[List[String]]= None) extends TStruct

object SponsorshipTargeting extends TStructCodec[SponsorshipTargeting]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,Option[CapiDateTime]]()

implicit val r2 = new TFieldCodec[w2.T,Option[List[String]]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w2.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.publishedSince,w2.value -> x.validEditions)

override def decode = (m) => for {
publishedSince <- m.get(w1.value).orElse(defaults.get(w1.value))

validEditions <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield SponsorshipTargeting(publishedSince,validEditions)
}