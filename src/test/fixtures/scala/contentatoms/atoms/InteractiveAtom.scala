package contentatom.interactive

case class InteractiveAtom(
  `type`:  String,
  title:   String,
  css:     String,
  html:    String,
  mainJS:  Option[String] = None,
  docData: Option[String] = None) extends TStruct

object InteractiveAtom extends TStructCodec[InteractiveAtom] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  val w6 = Witness(6)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, String]
  implicit val r4 = new TFieldCodec[w4.T, String]
  implicit val r5 = new TFieldCodec[w5.T, Option[String]]
  implicit val r6 = new TFieldCodec[w6.T, Option[String]]
  override val defaults = HMap[TFieldCodec](
    w5.value -> None,
    w6.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.`type`,
    w2.value -> x.title,
    w3.value -> x.css,
    w4.value -> x.html,
    w5.value -> x.mainJS,
    w6.value -> x.docData)
  override def decode = (m) ⇒ for {
    `type` ← m.get(w1.value).orElse(defaults.get(w1.value))
    title ← m.get(w2.value).orElse(defaults.get(w2.value))
    css ← m.get(w3.value).orElse(defaults.get(w3.value))
    html ← m.get(w4.value).orElse(defaults.get(w4.value))
    mainJS ← m.get(w5.value).orElse(defaults.get(w5.value))
    docData ← m.get(w6.value).orElse(defaults.get(w6.value))
  } yield InteractiveAtom(
    `type`,
    title,
    css,
    html,
    mainJS,
    docData)
}