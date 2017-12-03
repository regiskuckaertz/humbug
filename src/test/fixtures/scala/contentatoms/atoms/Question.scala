package contentatom.storyquestions

case class Question(questionId: String,questionText: String,answers: Option[List[Answer]]= Some(List())) extends TStruct

object Question extends TStructCodec[Question]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,String]()

implicit val r3 = new TFieldCodec[w3.T,Option[List[Answer]]]()

override val defaults = HMap[TFieldCodec](w3.value -> Some(List()))

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.questionId,w2.value -> x.questionText,w3.value -> x.answers)

override def decode = (m) => for {
questionId <- m.get(w1.value).orElse(defaults.get(w1.value))

questionText <- m.get(w2.value).orElse(defaults.get(w2.value))

answers <- m.get(w3.value).orElse(defaults.get(w3.value))
} yield Question(questionId,questionText,answers)
}