package com.gu.contentatom.thrift.atom.media

case class PlutoData(
  commissionId: Option[String] = None,
  projectId:    Option[String] = None,
  masterId:     Option[String] = None) extends TStruct

object PlutoData extends TStructCodec[PlutoData] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w3.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.commissionId,
    w2.value -> x.projectId,
    w3.value -> x.masterId)
  override def decode = (m) ⇒ for {
    commissionId ← m.get(w1.value).orElse(defaults.get(w1.value))
    projectId ← m.get(w2.value).orElse(defaults.get(w2.value))
    masterId ← m.get(w3.value).orElse(defaults.get(w3.value))
  } yield PlutoData(
    commissionId,
    projectId,
    masterId)
}