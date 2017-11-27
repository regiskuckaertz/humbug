package com.gu.contentapi.client.model.v1

case class CrosswordEntry(id: String, number: Option[Int] = None, humanNumber: Option[String] = None, direction: Option[String] = None, position: Option[CrosswordPosition] = None, separatorLocations: Option[Map[String, List[Int]]] = None, length: Option[Int] = None, clue: Option[String] = None, group: Option[List[String]] = None, solution: Option[String] = None, format: Option[String] = None) extends TStruct

object CrosswordEntry extends TStructCodec[CrosswordEntry] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  val w3 = Witness(3)

  val w4 = Witness(4)

  val w5 = Witness(5)

  val w6 = Witness(6)

  val w7 = Witness(7)

  val w8 = Witness(8)

  val w9 = Witness(9)

  val w10 = Witness(1)

  val w11 = Witness(11)

  implicit val r1 = new TFieldCodec[w1.T, String]()

  implicit val r2 = new TFieldCodec[w2.T, Option[Int]]()

  implicit val r3 = new TFieldCodec[w3.T, Option[String]]()

  implicit val r4 = new TFieldCodec[w4.T, Option[String]]()

  implicit val r5 = new TFieldCodec[w5.T, Option[CrosswordPosition]]()

  implicit val r6 = new TFieldCodec[w6.T, Option[Map[String, List[Int]]]]()

  implicit val r7 = new TFieldCodec[w7.T, Option[Int]]()

  implicit val r8 = new TFieldCodec[w8.T, Option[String]]()

  implicit val r9 = new TFieldCodec[w9.T, Option[List[String]]]()

  implicit val r10 = new TFieldCodec[w10.T, Option[String]]()

  implicit val r11 = new TFieldCodec[w11.T, Option[String]]()

  override val defaults = HMap[TFieldCodec](w2.value -> None, w3.value -> None, w4.value -> None, w5.value -> None, w6.value -> None, w7.value -> None, w8.value -> None, w9.value -> None, w10.value -> None, w11.value -> None)

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.id, w2.value -> x.number, w3.value -> x.humanNumber, w4.value -> x.direction, w5.value -> x.position, w6.value -> x.separatorLocations, w7.value -> x.length, w8.value -> x.clue, w9.value -> x.group, w10.value -> x.solution, w11.value -> x.format)

  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))

    number ← m.get(w2.value).orElse(defaults.get(w2.value))

    humanNumber ← m.get(w3.value).orElse(defaults.get(w3.value))

    direction ← m.get(w4.value).orElse(defaults.get(w4.value))

    position ← m.get(w5.value).orElse(defaults.get(w5.value))

    separatorLocations ← m.get(w6.value).orElse(defaults.get(w6.value))

    length ← m.get(w7.value).orElse(defaults.get(w7.value))

    clue ← m.get(w8.value).orElse(defaults.get(w8.value))

    group ← m.get(w9.value).orElse(defaults.get(w9.value))

    solution ← m.get(w10.value).orElse(defaults.get(w10.value))

    format ← m.get(w11.value).orElse(defaults.get(w11.value))
  } yield CrosswordEntry(id, number, humanNumber, direction, position, separatorLocations, length, clue, group, solution, format)
}