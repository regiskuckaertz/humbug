package com.gu.contentapi.client.model.v1

case class Atoms(quizzes: Option[List[Atom]]= None,viewpoints: Option[List[Atom]]= None,media: Option[List[Atom]]= None,explainers: Option[List[Atom]]= None,cta: Option[List[Atom]]= None,interactives: Option[List[Atom]]= None,reviews: Option[List[Atom]]= None,recipes: Option[List[Atom]]= None,storyquestions: Option[List[Atom]]= None,qandas: Option[List[Atom]]= None,guides: Option[List[Atom]]= None,profiles: Option[List[Atom]]= None,timelines: Option[List[Atom]]= None) extends TStruct

object Atoms extends TStructCodec[Atoms]{
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

implicit val r1 = new TFieldCodec[w1.T,Option[List[Atom]]]()

implicit val r2 = new TFieldCodec[w2.T,Option[List[Atom]]]()

implicit val r3 = new TFieldCodec[w3.T,Option[List[Atom]]]()

implicit val r4 = new TFieldCodec[w4.T,Option[List[Atom]]]()

implicit val r5 = new TFieldCodec[w5.T,Option[List[Atom]]]()

implicit val r6 = new TFieldCodec[w6.T,Option[List[Atom]]]()

implicit val r7 = new TFieldCodec[w7.T,Option[List[Atom]]]()

implicit val r8 = new TFieldCodec[w8.T,Option[List[Atom]]]()

implicit val r9 = new TFieldCodec[w9.T,Option[List[Atom]]]()

implicit val r10 = new TFieldCodec[w10.T,Option[List[Atom]]]()

implicit val r11 = new TFieldCodec[w11.T,Option[List[Atom]]]()

implicit val r12 = new TFieldCodec[w12.T,Option[List[Atom]]]()

implicit val r13 = new TFieldCodec[w13.T,Option[List[Atom]]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w2.value -> None,w3.value -> None,w4.value -> None,w5.value -> None,w6.value -> None,w7.value -> None,w8.value -> None,w9.value -> None,w10.value -> None,w11.value -> None,w12.value -> None,w13.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.quizzes,w2.value -> x.viewpoints,w3.value -> x.media,w4.value -> x.explainers,w5.value -> x.cta,w6.value -> x.interactives,w7.value -> x.reviews,w8.value -> x.recipes,w9.value -> x.storyquestions,w10.value -> x.qandas,w11.value -> x.guides,w12.value -> x.profiles,w13.value -> x.timelines)

override def decode = (m) => for {
quizzes <- m.get(w1.value).orElse(defaults.get(w1.value))

viewpoints <- m.get(w2.value).orElse(defaults.get(w2.value))

media <- m.get(w3.value).orElse(defaults.get(w3.value))

explainers <- m.get(w4.value).orElse(defaults.get(w4.value))

cta <- m.get(w5.value).orElse(defaults.get(w5.value))

interactives <- m.get(w6.value).orElse(defaults.get(w6.value))

reviews <- m.get(w7.value).orElse(defaults.get(w7.value))

recipes <- m.get(w8.value).orElse(defaults.get(w8.value))

storyquestions <- m.get(w9.value).orElse(defaults.get(w9.value))

qandas <- m.get(w10.value).orElse(defaults.get(w10.value))

guides <- m.get(w11.value).orElse(defaults.get(w11.value))

profiles <- m.get(w12.value).orElse(defaults.get(w12.value))

timelines <- m.get(w13.value).orElse(defaults.get(w13.value))
} yield Atoms(quizzes,viewpoints,media,explainers,cta,interactives,reviews,recipes,storyquestions,qandas,guides,profiles,timelines)
}