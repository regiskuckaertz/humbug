package com.gu.contentapi.client.model.v1

case class Blocks(
  main:                Option[Block]                    = None,
  body:                Option[List[Block]]              = None,
  totalBodyBlocks:     Option[Int]                      = None,
  requestedBodyBlocks: Option[Map[String, List[Block]]] = None) extends TStruct

object Blocks extends TStructCodec[Blocks] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  implicit val r1 = new TFieldCodec[w1.T, Option[Block]]
  implicit val r2 = new TFieldCodec[w2.T, Option[List[Block]]]
  implicit val r3 = new TFieldCodec[w3.T, Option[Int]]
  implicit val r4 = new TFieldCodec[w4.T, Option[Map[String, List[Block]]]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w3.value -> None,
    w4.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.main,
    w2.value -> x.body,
    w3.value -> x.totalBodyBlocks,
    w4.value -> x.requestedBodyBlocks)
  override def decode = (m) ⇒ for {
    main ← m.get(w1.value).orElse(defaults.get(w1.value))
    body ← m.get(w2.value).orElse(defaults.get(w2.value))
    totalBodyBlocks ← m.get(w3.value).orElse(defaults.get(w3.value))
    requestedBodyBlocks ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield Blocks(
    main,
    body,
    totalBodyBlocks,
    requestedBodyBlocks)
}