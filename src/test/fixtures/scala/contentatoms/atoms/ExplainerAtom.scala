package contentatom.explainer

case class ExplainerAtom(
  title:       String,
  body:        String,
  displayType: DisplayType,
  tags:        Option[List[String]] = None) extends TStruct

object ExplainerAtom extends TStructCodec[ExplainerAtom] {
  val w1 = Witness(2)
  val w2 = Witness(3)
  val w3 = Witness(4)
  val w4 = Witness(5)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, DisplayType]
  implicit val r4 = new TFieldCodec[w4.T, Option[List[String]]]
  override val defaults = HMap[TFieldCodec](w4.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.title,
    w2.value -> x.body,
    w3.value -> x.displayType,
    w4.value -> x.tags)
  override def decode = (m) ⇒ for {
    title ← m.get(w1.value).orElse(defaults.get(w1.value))
    body ← m.get(w2.value).orElse(defaults.get(w2.value))
    displayType ← m.get(w3.value).orElse(defaults.get(w3.value))
    tags ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield ExplainerAtom(
    title,
    body,
    displayType,
    tags)
}