package com.gu.contentapi.client.model.v1

case class Pillar(id: String, name: String, sectionIds: List[String]) extends TStruct

object Pillar extends TStructCodec[Pillar] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  val w3 = Witness(3)

  implicit val r1 = new TFieldCodec[w1.T, String]()

  implicit val r2 = new TFieldCodec[w2.T, String]()

  implicit val r3 = new TFieldCodec[w3.T, List[String]]()

  override val defaults = HMap[TFieldCodec]()

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.id, w2.value -> x.name, w3.value -> x.sectionIds)

  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))

    name ← m.get(w2.value).orElse(defaults.get(w2.value))

    sectionIds ← m.get(w3.value).orElse(defaults.get(w3.value))
  } yield Pillar(id, name, sectionIds)
}