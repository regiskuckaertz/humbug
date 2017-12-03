package com.gu.contententity.thrift.entity.place

case class Place(name: String) extends TStruct

object Place extends TStructCodec[Place] {
  val w1 = Witness(1)
  implicit val r1 = new TFieldCodec[w1.T, String]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.name)
  override def decode = (m) ⇒ for {
    name ← m.get(w1.value).orElse(defaults.get(w1.value))
  } yield Place(name)
}