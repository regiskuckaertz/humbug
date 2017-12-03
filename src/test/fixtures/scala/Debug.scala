package com.gu.contentapi.client.model.v1

case class Debug(lastSeenByPorterAt: Option[CapiDateTime]= None,revisionSeenByPorter: Option[Long]= None,contentSource: Option[String]= None) extends TStruct

object Debug extends TStructCodec[Debug]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

implicit val r1 = new TFieldCodec[w1.T,Option[CapiDateTime]]()

implicit val r2 = new TFieldCodec[w2.T,Option[Long]]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w2.value -> None,w3.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.lastSeenByPorterAt,w2.value -> x.revisionSeenByPorter,w3.value -> x.contentSource)

override def decode = (m) => for {
lastSeenByPorterAt <- m.get(w1.value).orElse(defaults.get(w1.value))

revisionSeenByPorter <- m.get(w2.value).orElse(defaults.get(w2.value))

contentSource <- m.get(w3.value).orElse(defaults.get(w3.value))
} yield Debug(lastSeenByPorterAt,revisionSeenByPorter,contentSource)
}