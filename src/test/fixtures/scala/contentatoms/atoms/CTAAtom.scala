package contentatom.cta

case class CTAAtom(
  url:             String,
  backgroundImage: Option[String] = None,
  btnText:         Option[String] = None,
  label:           Option[String] = None,
  trackingCode:    Option[String] = None) extends TStruct

object CTAAtom extends TStructCodec[CTAAtom] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[String]]
  implicit val r5 = new TFieldCodec[w5.T, Option[String]]
  override val defaults = HMap[TFieldCodec](
    w2.value -> None,
    w3.value -> None,
    w4.value -> None,
    w5.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.url,
    w2.value -> x.backgroundImage,
    w3.value -> x.btnText,
    w4.value -> x.label,
    w5.value -> x.trackingCode)
  override def decode = (m) ⇒ for {
    url ← m.get(w1.value).orElse(defaults.get(w1.value))
    backgroundImage ← m.get(w2.value).orElse(defaults.get(w2.value))
    btnText ← m.get(w3.value).orElse(defaults.get(w3.value))
    label ← m.get(w4.value).orElse(defaults.get(w4.value))
    trackingCode ← m.get(w5.value).orElse(defaults.get(w5.value))
  } yield CTAAtom(
    url,
    backgroundImage,
    btnText,
    label,
    trackingCode)
}