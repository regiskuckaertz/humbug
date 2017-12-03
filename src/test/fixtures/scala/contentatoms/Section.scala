package com.gu.contentatom.thrift

case class Section(
  id:         Long,
  name:       Option[String] = None,
  pathPrefix: Option[String] = None,
  slug:       Option[String] = None) extends TStruct

object Section extends TStructCodec[Section] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  implicit val r1 = new TFieldCodec[w1.T, Long]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  override val defaults = HMap[TFieldCodec](
    w2.value -> None,
    w3.value -> None,
    w4.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.id,
    w2.value -> x.name,
    w3.value -> x.pathPrefix,
    w4.value -> x.slug)
  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))
    name ← m.get(w2.value).orElse(defaults.get(w2.value))
    pathPrefix ← m.get(w3.value).orElse(defaults.get(w3.value))
    slug ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield Section(
    id,
    name,
    pathPrefix,
    slug)
}