package com.gu.contentatom.thrift

case class EmailProvider(
  name:   String = "xact-targe",
  listId: String) extends TStruct

object EmailProvider extends TStructCodec[EmailProvider] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  override val defaults = HMap[TFieldCodec](w1.value -> "xact-targe")
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.name,
    w2.value -> x.listId)
  override def decode = (m) ⇒ for {
    name ← m.get(w1.value).orElse(defaults.get(w1.value))
    listId ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield EmailProvider(
    name,
    listId)
}