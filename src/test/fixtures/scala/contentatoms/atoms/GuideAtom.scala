package contentatom.guide

case class GuideAtom(
  typeLabel:  Option[String]  = None,
  guideImage: Option[Image]   = None,
  items:      List[GuideItem]) extends TStruct

object GuideAtom extends TStructCodec[GuideAtom] {
  val w1 = Witness(1)
  val w2 = Witness(3)
  val w3 = Witness(4)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, Option[Image]]
  implicit val r3 = new TFieldCodec[w3.T, List[GuideItem]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.typeLabel,
    w2.value -> x.guideImage,
    w3.value -> x.items)
  override def decode = (m) ⇒ for {
    typeLabel ← m.get(w1.value).orElse(defaults.get(w1.value))
    guideImage ← m.get(w2.value).orElse(defaults.get(w2.value))
    items ← m.get(w3.value).orElse(defaults.get(w3.value))
  } yield GuideAtom(
    typeLabel,
    guideImage,
    items)
}