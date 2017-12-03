package contentatom.quiz

case class QuizContent(questions: List[Question],resultGroups: Option[ResultGroups]= None,resultBuckets: Option[ResultBuckets]= None) extends TStruct

object QuizContent extends TStructCodec[QuizContent]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

implicit val r1 = new TFieldCodec[w1.T,List[Question]]()

implicit val r2 = new TFieldCodec[w2.T,Option[ResultGroups]]()

implicit val r3 = new TFieldCodec[w3.T,Option[ResultBuckets]]()

override val defaults = HMap[TFieldCodec](w2.value -> None,w3.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.questions,w2.value -> x.resultGroups,w3.value -> x.resultBuckets)

override def decode = (m) => for {
questions <- m.get(w1.value).orElse(defaults.get(w1.value))

resultGroups <- m.get(w2.value).orElse(defaults.get(w2.value))

resultBuckets <- m.get(w3.value).orElse(defaults.get(w3.value))
} yield QuizContent(questions,resultGroups,resultBuckets)
}