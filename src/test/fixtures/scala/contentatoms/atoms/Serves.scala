package contentatom.recipe

case class Serves(
  `type`: String,
  from:   Short,
  to:     Short,
  unit:   Option[String] = None) extends TStruct

object Serves extends TStructCodec[Serves] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, Short]
  implicit val r3 = new TFieldCodec[w3.T, Short]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  override val defaults = HMap[TFieldCodec](w4.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.`type`,
    w2.value -> x.from,
    w3.value -> x.to,
    w4.value -> x.unit)
  override def decode = (m) ⇒ for {
    `type` ← m.get(w1.value).orElse(defaults.get(w1.value))
    from ← m.get(w2.value).orElse(defaults.get(w2.value))
    to ← m.get(w3.value).orElse(defaults.get(w3.value))
    unit ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield Serves(
    `type`,
    from,
    to,
    unit)
}