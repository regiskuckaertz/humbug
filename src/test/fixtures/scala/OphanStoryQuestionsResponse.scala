package com.gu.contentapi.client.model.v1

case class OphanStoryQuestionsResponse(status: String,pathToAtomIds: List[PathAndStoryQuestionsAtomId]) extends TStruct

object OphanStoryQuestionsResponse extends TStructCodec[OphanStoryQuestionsResponse]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,List[PathAndStoryQuestionsAtomId]]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.status,w2.value -> x.pathToAtomIds)

override def decode = (m) => for {
status <- m.get(w1.value).orElse(defaults.get(w1.value))

pathToAtomIds <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield OphanStoryQuestionsResponse(status,pathToAtomIds)
}