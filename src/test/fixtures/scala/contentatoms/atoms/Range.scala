package contentatom.recipe

case class Range(
  from: Short,
  to:   Short) extends TStruct

object Range extends TStructCodec[Range] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, Short]
  implicit val r2 = new TFieldCodec[w2.T, Short]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.from,
    w2.value -> x.to)
  override def decode = (m) ⇒ for {
    from ← m.get(w1.value).orElse(defaults.get(w1.value))
    to ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield Range(
    from,
    to)
}