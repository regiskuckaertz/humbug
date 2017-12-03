package contentatom.storyquestions

case class QuestionSet(questionSetId: String,questionSetTitle: String,questions: List[Question]) extends TStruct

object QuestionSet extends TStructCodec[QuestionSet]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,String]()

implicit val r3 = new TFieldCodec[w3.T,List[Question]]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.questionSetId,w2.value -> x.questionSetTitle,w3.value -> x.questions)

override def decode = (m) => for {
questionSetId <- m.get(w1.value).orElse(defaults.get(w1.value))

questionSetTitle <- m.get(w2.value).orElse(defaults.get(w2.value))

questions <- m.get(w3.value).orElse(defaults.get(w3.value))
} yield QuestionSet(questionSetId,questionSetTitle,questions)
}