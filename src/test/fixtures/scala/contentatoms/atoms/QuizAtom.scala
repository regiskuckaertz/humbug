package contentatom.quiz

case class QuizAtom(
  id:             String,
  title:          String,
  revealAtEnd:    Boolean,
  published:      Boolean,
  quizType:       String,
  defaultColumns: Option[Short] = None,
  content:        QuizContent) extends TStruct

object QuizAtom extends TStructCodec[QuizAtom] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(6)
  val w4 = Witness(7)
  val w5 = Witness(8)
  val w6 = Witness(9)
  val w7 = Witness(1)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, Boolean]
  implicit val r4 = new TFieldCodec[w4.T, Boolean]
  implicit val r5 = new TFieldCodec[w5.T, String]
  implicit val r6 = new TFieldCodec[w6.T, Option[Short]]
  implicit val r7 = new TFieldCodec[w7.T, QuizContent]
  override val defaults = HMap[TFieldCodec](w6.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.id,
    w2.value -> x.title,
    w3.value -> x.revealAtEnd,
    w4.value -> x.published,
    w5.value -> x.quizType,
    w6.value -> x.defaultColumns,
    w7.value -> x.content)
  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))
    title ← m.get(w2.value).orElse(defaults.get(w2.value))
    revealAtEnd ← m.get(w3.value).orElse(defaults.get(w3.value))
    published ← m.get(w4.value).orElse(defaults.get(w4.value))
    quizType ← m.get(w5.value).orElse(defaults.get(w5.value))
    defaultColumns ← m.get(w6.value).orElse(defaults.get(w6.value))
    content ← m.get(w7.value).orElse(defaults.get(w7.value))
  } yield QuizAtom(
    id,
    title,
    revealAtEnd,
    published,
    quizType,
    defaultColumns,
    content)
}