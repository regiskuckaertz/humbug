package com.gu.contentatom.thrift

sealed trait AtomData extends TUnion


case class Quiz(quiz: QuizAtom) extends AtomData

case class Media(media: MediaAtom) extends AtomData

case class Explainer(explainer: ExplainerAtom) extends AtomData

case class Cta(cta: CTAAtom) extends AtomData

case class Interactive(interactive: InteractiveAtom) extends AtomData

case class Review(review: ReviewAtom) extends AtomData

case class Recipe(recipe: RecipeAtom) extends AtomData

case class Storyquestions(storyquestions: StoryQuestionsAtom) extends AtomData

case class Qanda(qanda: QAndAAtom) extends AtomData

case class Guide(guide: GuideAtom) extends AtomData

case class Profile(profile: ProfileAtom) extends AtomData

case class Timeline(timeline: TimelineAtom) extends AtomData

object AtomData extends TUnionCodec[AtomData]{
val w1 = Witness(1)

val w2 = Witness(3)

val w3 = Witness(4)

val w4 = Witness(5)

val w5 = Witness(6)

val w6 = Witness(7)

val w7 = Witness(8)

val w8 = Witness(9)

val w9 = Witness(1)

val w10 = Witness(11)

val w11 = Witness(21)

val w12 = Witness(31)

implicit val r1 = new TFieldCodec[w1.T,QuizAtom]()

implicit val r2 = new TFieldCodec[w2.T,MediaAtom]()

implicit val r3 = new TFieldCodec[w3.T,ExplainerAtom]()

implicit val r4 = new TFieldCodec[w4.T,CTAAtom]()

implicit val r5 = new TFieldCodec[w5.T,InteractiveAtom]()

implicit val r6 = new TFieldCodec[w6.T,ReviewAtom]()

implicit val r7 = new TFieldCodec[w7.T,RecipeAtom]()

implicit val r8 = new TFieldCodec[w8.T,StoryQuestionsAtom]()

implicit val r9 = new TFieldCodec[w9.T,QAndAAtom]()

implicit val r10 = new TFieldCodec[w10.T,GuideAtom]()

implicit val r11 = new TFieldCodec[w11.T,ProfileAtom]()

implicit val r12 = new TFieldCodec[w12.T,TimelineAtom]()

override def encode = {
case Quiz(x) => HMap[TFieldCodec](w1.value -> x)

case Media(x) => HMap[TFieldCodec](w2.value -> x)

case Explainer(x) => HMap[TFieldCodec](w3.value -> x)

case Cta(x) => HMap[TFieldCodec](w4.value -> x)

case Interactive(x) => HMap[TFieldCodec](w5.value -> x)

case Review(x) => HMap[TFieldCodec](w6.value -> x)

case Recipe(x) => HMap[TFieldCodec](w7.value -> x)

case Storyquestions(x) => HMap[TFieldCodec](w8.value -> x)

case Qanda(x) => HMap[TFieldCodec](w9.value -> x)

case Guide(x) => HMap[TFieldCodec](w10.value -> x)

case Profile(x) => HMap[TFieldCodec](w11.value -> x)

case Timeline(x) => HMap[TFieldCodec](w12.value -> x)
}

override def decode = (m) => m.get(w1.value).map(Quiz(`_`)).orElse(m.get(w2.value).map(Media(`_`)).orElse(m.get(w3.value).map(Explainer(`_`)).orElse(m.get(w4.value).map(Cta(`_`)).orElse(m.get(w5.value).map(Interactive(`_`)).orElse(m.get(w6.value).map(Review(`_`)).orElse(m.get(w7.value).map(Recipe(`_`)).orElse(m.get(w8.value).map(Storyquestions(`_`)).orElse(m.get(w9.value).map(Qanda(`_`)).orElse(m.get(w10.value).map(Guide(`_`)).orElse(m.get(w11.value).map(Profile(`_`)).orElse(m.get(w12.value).map(Timeline(`_`)))))))))))))
}