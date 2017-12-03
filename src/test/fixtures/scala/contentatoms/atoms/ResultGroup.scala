package contentatom.quiz

case class ResultGroup(
  title:    String,
  share:    String,
  minScore: Short,
  id:       String) extends TStruct

object ResultGroup extends TStructCodec[ResultGroup] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, Short]
  implicit val r4 = new TFieldCodec[w4.T, String]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.title,
    w2.value -> x.share,
    w3.value -> x.minScore,
    w4.value -> x.id)
  override def decode = (m) ⇒ for {
    title ← m.get(w1.value).orElse(defaults.get(w1.value))
    share ← m.get(w2.value).orElse(defaults.get(w2.value))
    minScore ← m.get(w3.value).orElse(defaults.get(w3.value))
    id ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield ResultGroup(
    title,
    share,
    minScore,
    id)
}