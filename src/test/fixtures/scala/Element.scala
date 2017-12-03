package com.gu.contentapi.client.model.v1

case class Element(
  id:           String,
  relation:     String,
  `type`:       ElementType,
  galleryIndex: Option[Int] = None,
  assets:       List[Asset]) extends TStruct

object Element extends TStructCodec[Element] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, ElementType]
  implicit val r4 = new TFieldCodec[w4.T, Option[Int]]
  implicit val r5 = new TFieldCodec[w5.T, List[Asset]]
  override val defaults = HMap[TFieldCodec](w4.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.id,
    w2.value -> x.relation,
    w3.value -> x.`type`,
    w4.value -> x.galleryIndex,
    w5.value -> x.assets)
  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))
    relation ← m.get(w2.value).orElse(defaults.get(w2.value))
    `type` ← m.get(w3.value).orElse(defaults.get(w3.value))
    galleryIndex ← m.get(w4.value).orElse(defaults.get(w4.value))
    assets ← m.get(w5.value).orElse(defaults.get(w5.value))
  } yield Element(
    id,
    relation,
    `type`,
    galleryIndex,
    assets)
}