package com.gu.contentapi.client.model.v1

case class EditionsResponse(status: String,userTier: String,total: Int,results: List[NetworkFront]) extends TStruct

object EditionsResponse extends TStructCodec[EditionsResponse]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,String]()

implicit val r3 = new TFieldCodec[w3.T,Int]()

implicit val r4 = new TFieldCodec[w4.T,List[NetworkFront]]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.status,w2.value -> x.userTier,w3.value -> x.total,w4.value -> x.results)

override def decode = (m) => for {
status <- m.get(w1.value).orElse(defaults.get(w1.value))

userTier <- m.get(w2.value).orElse(defaults.get(w2.value))

total <- m.get(w3.value).orElse(defaults.get(w3.value))

results <- m.get(w4.value).orElse(defaults.get(w4.value))
} yield EditionsResponse(status,userTier,total,results)
}