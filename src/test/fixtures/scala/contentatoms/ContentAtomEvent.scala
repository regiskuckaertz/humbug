package com.gu.contentatom.thrift

case class ContentAtomEvent(
  atom:              Atom,
  eventType:         EventType,
  eventCreationTime: DateTime) extends TStruct

object ContentAtomEvent extends TStructCodec[ContentAtomEvent] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  implicit val r1 = new TFieldCodec[w1.T, Atom]
  implicit val r2 = new TFieldCodec[w2.T, EventType]
  implicit val r3 = new TFieldCodec[w3.T, DateTime]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.atom,
    w2.value -> x.eventType,
    w3.value -> x.eventCreationTime)
  override def decode = (m) ⇒ for {
    atom ← m.get(w1.value).orElse(defaults.get(w1.value))
    eventType ← m.get(w2.value).orElse(defaults.get(w2.value))
    eventCreationTime ← m.get(w3.value).orElse(defaults.get(w3.value))
  } yield ContentAtomEvent(
    atom,
    eventType,
    eventCreationTime)
}