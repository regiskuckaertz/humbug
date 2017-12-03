package com.gu.contentatom.thrift

case class Taxonomy(
  tags:         Option[List[TagUsage]]  = None,
  contributors: Option[List[Tag]]       = None,
  publication:  Option[Tag]             = None,
  newspaper:    Option[Newspaper]       = None,
  references:   Option[List[Reference]] = None) extends TStruct

object Taxonomy extends TStructCodec[Taxonomy] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  implicit val r1 = new TFieldCodec[w1.T, Option[List[TagUsage]]]
  implicit val r2 = new TFieldCodec[w2.T, Option[List[Tag]]]
  implicit val r3 = new TFieldCodec[w3.T, Option[Tag]]
  implicit val r4 = new TFieldCodec[w4.T, Option[Newspaper]]
  implicit val r5 = new TFieldCodec[w5.T, Option[List[Reference]]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w3.value -> None,
    w4.value -> None,
    w5.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.tags,
    w2.value -> x.contributors,
    w3.value -> x.publication,
    w4.value -> x.newspaper,
    w5.value -> x.references)
  override def decode = (m) ⇒ for {
    tags ← m.get(w1.value).orElse(defaults.get(w1.value))
    contributors ← m.get(w2.value).orElse(defaults.get(w2.value))
    publication ← m.get(w3.value).orElse(defaults.get(w3.value))
    newspaper ← m.get(w4.value).orElse(defaults.get(w4.value))
    references ← m.get(w5.value).orElse(defaults.get(w5.value))
  } yield Taxonomy(
    tags,
    contributors,
    publication,
    newspaper,
    references)
}