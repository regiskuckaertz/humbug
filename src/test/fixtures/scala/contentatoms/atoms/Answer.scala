package com.gu.contentatom.thrift.atom.storyquestions

case class Answer(
  answerId:   String,
  answerType: AnswerType) extends TStruct

object Answer extends TStructCodec[Answer] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, AnswerType]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.answerId,
    w2.value -> x.answerType)
  override def decode = (m) ⇒ for {
    answerId ← m.get(w1.value).orElse(defaults.get(w1.value))
    answerType ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield Answer(
    answerId,
    answerType)
}