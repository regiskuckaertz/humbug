package com.gu.contentapi.client.model.v1

case class CapiDateTime(dateTime: Long, iso8601: String) extends TStruct

object CapiDateTime extends TStructCodec[CapiDateTime] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  implicit val r1 = new TFieldCodec[w1.T, Long]()

  implicit val r2 = new TFieldCodec[w2.T, String]()

  override val defaults = HMap[TFieldCodec]()

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.dateTime, w2.value -> x.iso8601)

  override def decode = (m) ⇒ for {
    dateTime ← m.get(w1.value).orElse(defaults.get(w1.value))

    iso8601 ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield CapiDateTime(dateTime, iso8601)
}