package contentatom.profile

case class ProfileAtom(
  typeLabel: Option[String]    = None,
  headshot:  Option[Image]     = None,
  items:     List[ProfileItem],
  entity:    Option[Entity]    = None) extends TStruct

object ProfileAtom extends TStructCodec[ProfileAtom] {
  val w1 = Witness(1)
  val w2 = Witness(3)
  val w3 = Witness(4)
  val w4 = Witness(5)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, Option[Image]]
  implicit val r3 = new TFieldCodec[w3.T, List[ProfileItem]]
  implicit val r4 = new TFieldCodec[w4.T, Option[Entity]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w4.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.typeLabel,
    w2.value -> x.headshot,
    w3.value -> x.items,
    w4.value -> x.entity)
  override def decode = (m) ⇒ for {
    typeLabel ← m.get(w1.value).orElse(defaults.get(w1.value))
    headshot ← m.get(w2.value).orElse(defaults.get(w2.value))
    items ← m.get(w3.value).orElse(defaults.get(w3.value))
    entity ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield ProfileAtom(
    typeLabel,
    headshot,
    items,
    entity)
}