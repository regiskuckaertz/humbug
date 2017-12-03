package contentatom.qanda

case class QAndAItem(
  title: Option[String] = None,
  body:  String) extends TStruct

object QAndAItem extends TStructCodec[QAndAItem] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, String]
  override val defaults = HMap[TFieldCodec](w1.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.title,
    w2.value -> x.body)
  override def decode = (m) ⇒ for {
    title ← m.get(w1.value).orElse(defaults.get(w1.value))
    body ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield QAndAItem(
    title,
    body)
}