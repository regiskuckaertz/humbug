package com.gu.contentapi.client.model.v1

case class PillarsResponse(status: String, total: Int, results: List[Pillar]) extends TStruct

object PillarsResponse extends TStructCodec[PillarsResponse] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  val w3 = Witness(3)

  implicit val r1 = new TFieldCodec[w1.T, String]()

  implicit val r2 = new TFieldCodec[w2.T, Int]()

  implicit val r3 = new TFieldCodec[w3.T, List[Pillar]]()

  override val defaults = HMap[TFieldCodec]()

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.status, w2.value -> x.total, w3.value -> x.results)

  override def decode = (m) ⇒ for {
    status ← m.get(w1.value).orElse(defaults.get(w1.value))

    total ← m.get(w2.value).orElse(defaults.get(w2.value))

    results ← m.get(w3.value).orElse(defaults.get(w3.value))
  } yield PillarsResponse(status, total, results)
}