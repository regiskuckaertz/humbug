package com.gu.contentapi.client.model.v1

case class Crossword(name: String, `type`: CrosswordType, number: Int, date: CapiDateTime, dimensions: CrosswordDimensions, entries: List[CrosswordEntry], solutionAvailable: Boolean, hasNumbers: Boolean, randomCluesOrdering: Boolean, instructions: Option[String] = None, creator: Option[CrosswordCreator] = None, pdf: Option[String] = None, annotatedSolution: Option[String] = None, dateSolutionAvailable: Option[CapiDateTime] = None) extends TStruct

object Crossword extends TStructCodec[Crossword] {
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

  val w12 = Witness(21)

  val w13 = Witness(31)

  val w14 = Witness(41)

  implicit val r1 = new TFieldCodec[w1.T, String]()

  implicit val r2 = new TFieldCodec[w2.T, CrosswordType]()

  implicit val r3 = new TFieldCodec[w3.T, Int]()

  implicit val r4 = new TFieldCodec[w4.T, CapiDateTime]()

  implicit val r5 = new TFieldCodec[w5.T, CrosswordDimensions]()

  implicit val r6 = new TFieldCodec[w6.T, List[CrosswordEntry]]()

  implicit val r7 = new TFieldCodec[w7.T, Boolean]()

  implicit val r8 = new TFieldCodec[w8.T, Boolean]()

  implicit val r9 = new TFieldCodec[w9.T, Boolean]()

  implicit val r10 = new TFieldCodec[w10.T, Option[String]]()

  implicit val r11 = new TFieldCodec[w11.T, Option[CrosswordCreator]]()

  implicit val r12 = new TFieldCodec[w12.T, Option[String]]()

  implicit val r13 = new TFieldCodec[w13.T, Option[String]]()

  implicit val r14 = new TFieldCodec[w14.T, Option[CapiDateTime]]()

  override val defaults = HMap[TFieldCodec](w10.value -> None, w11.value -> None, w12.value -> None, w13.value -> None, w14.value -> None)

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.name, w2.value -> x.`type`, w3.value -> x.number, w4.value -> x.date, w5.value -> x.dimensions, w6.value -> x.entries, w7.value -> x.solutionAvailable, w8.value -> x.hasNumbers, w9.value -> x.randomCluesOrdering, w10.value -> x.instructions, w11.value -> x.creator, w12.value -> x.pdf, w13.value -> x.annotatedSolution, w14.value -> x.dateSolutionAvailable)

  override def decode = (m) ⇒ for {
    name ← m.get(w1.value).orElse(defaults.get(w1.value))

    `type` ← m.get(w2.value).orElse(defaults.get(w2.value))

    number ← m.get(w3.value).orElse(defaults.get(w3.value))

    date ← m.get(w4.value).orElse(defaults.get(w4.value))

    dimensions ← m.get(w5.value).orElse(defaults.get(w5.value))

    entries ← m.get(w6.value).orElse(defaults.get(w6.value))

    solutionAvailable ← m.get(w7.value).orElse(defaults.get(w7.value))

    hasNumbers ← m.get(w8.value).orElse(defaults.get(w8.value))

    randomCluesOrdering ← m.get(w9.value).orElse(defaults.get(w9.value))

    instructions ← m.get(w10.value).orElse(defaults.get(w10.value))

    creator ← m.get(w11.value).orElse(defaults.get(w11.value))

    pdf ← m.get(w12.value).orElse(defaults.get(w12.value))

    annotatedSolution ← m.get(w13.value).orElse(defaults.get(w13.value))

    dateSolutionAvailable ← m.get(w14.value).orElse(defaults.get(w14.value))
  } yield Crossword(name, `type`, number, date, dimensions, entries, solutionAvailable, hasNumbers, randomCluesOrdering, instructions, creator, pdf, annotatedSolution, dateSolutionAvailable)
}