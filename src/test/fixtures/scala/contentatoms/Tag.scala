package com.gu.contentatom.thrift

case class Tag(
  id:           Long,
  `type`:       Option[String]  = None,
  internalName: Option[String]  = None,
  externalName: Option[String]  = None,
  slug:         Option[String]  = None,
  section:      Option[Section] = None,
  path:         Option[String]  = None) extends TStruct

object Tag extends TStructCodec[Tag] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  val w6 = Witness(6)
  val w7 = Witness(7)
  implicit val r1 = new TFieldCodec[w1.T, Long]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  implicit val r5 = new TFieldCodec[w5.T, Option[String]]
  implicit val r6 = new TFieldCodec[w6.T, Option[Section]]
  implicit val r7 = new TFieldCodec[w7.T, Option[String]]
  override val defaults = HMap[TFieldCodec](
    w2.value -> None,
    w3.value -> None,
    w4.value -> None,
    w5.value -> None,
    w6.value -> None,
    w7.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.id,
    w2.value -> x.`type`,
    w3.value -> x.internalName,
    w4.value -> x.externalName,
    w5.value -> x.slug,
    w6.value -> x.section,
    w7.value -> x.path)
  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))
    `type` ← m.get(w2.value).orElse(defaults.get(w2.value))
    internalName ← m.get(w3.value).orElse(defaults.get(w3.value))
    externalName ← m.get(w4.value).orElse(defaults.get(w4.value))
    slug ← m.get(w5.value).orElse(defaults.get(w5.value))
    section ← m.get(w6.value).orElse(defaults.get(w6.value))
    path ← m.get(w7.value).orElse(defaults.get(w7.value))
  } yield Tag(
    id,
    `type`,
    internalName,
    externalName,
    slug,
    section,
    path)
}