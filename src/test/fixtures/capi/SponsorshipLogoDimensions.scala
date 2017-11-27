package com.gu.contentapi.client.model.v1

case class SponsorshipLogoDimensions(width: Int, height: Int) extends TStruct

object SponsorshipLogoDimensions extends TStructCodec[SponsorshipLogoDimensions] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  implicit val r1 = new TFieldCodec[w1.T, Int]()

  implicit val r2 = new TFieldCodec[w2.T, Int]()

  override val defaults = HMap[TFieldCodec]()

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.width, w2.value -> x.height)

  override def decode = (m) ⇒ for {
    width ← m.get(w1.value).orElse(defaults.get(w1.value))

    height ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield SponsorshipLogoDimensions(width, height)
}