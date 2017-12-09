package com.gu.contentatom.thrift.atom.qanda

case class QAndAAtom(
  typeLabel:  Option[String]   = None,
  eventImage: Option[Image]    = None,
  item:       QAndAItem,
  question:   Option[Question] = None) extends TStruct

object QAndAAtom extends TStructCodec[QAndAAtom] {
  val w1 = Witness(1)
  val w2 = Witness(3)
  val w3 = Witness(4)
  val w4 = Witness(5)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, Option[Image]]
  implicit val r3 = new TFieldCodec[w3.T, QAndAItem]
  implicit val r4 = new TFieldCodec[w4.T, Option[Question]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w4.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.typeLabel,
    w2.value -> x.eventImage,
    w3.value -> x.item,
    w4.value -> x.question)
  override def decode = (m) ⇒ for {
    typeLabel ← m.get(w1.value).orElse(defaults.get(w1.value))
    eventImage ← m.get(w2.value).orElse(defaults.get(w2.value))
    item ← m.get(w3.value).orElse(defaults.get(w3.value))
    question ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield QAndAAtom(
    typeLabel,
    eventImage,
    item,
    question)
}