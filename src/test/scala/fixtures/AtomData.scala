package test.fixtures

import humbug._
import humbug.codecs._

sealed trait AtomData[A] extends TUnion
case class AtomData$QuizAtom(quiz: Int) extends AtomData[Int]
case class AtomData$MediaAtom(media: Int) extends AtomData[Int]
case class AtomData$ExplainerAtom(explainer: Int) extends AtomData[Int]
case class AtomData$CTAAtom(cta: Int) extends AtomData[Int]
case class AtomData$InteractiveAtom(interactive: Int) extends AtomData[Int]
case class AtomData$ReviewAtom(review: Int) extends AtomData[Int]
case class AtomData$RecipeAtom(recipe: Int) extends AtomData[Int]
case class AtomData$StoryQuestionsAtom(storyquestions: Int) extends AtomData[Int]
case class AtomData$QAndAAtom(qanda: Int) extends AtomData[Int]
case class AtomData$GuideAtom(guide: Int) extends AtomData[Int]
case class AtomData$ProfileAtom(profile: Int) extends AtomData[Int]
case class AtomData$TimelineAtom(timeline: Int) extends AtomData[Int]

object AtomData {
  implicit val codec = new TUnionCodec[AtomData[_]] {
    def encode = {
      case AtomData$QuizAtom(x)           ⇒ (-1, Dyn(x, TyI32))
      case AtomData$MediaAtom(x)          ⇒ (-2, Dyn(x, TyI32))
      case AtomData$ExplainerAtom(x)      ⇒ (-3, Dyn(x, TyI32))
      case AtomData$CTAAtom(x)            ⇒ (-4, Dyn(x, TyI32))
      case AtomData$InteractiveAtom(x)    ⇒ (-5, Dyn(x, TyI32))
      case AtomData$ReviewAtom(x)         ⇒ (-6, Dyn(x, TyI32))
      case AtomData$RecipeAtom(x)         ⇒ (-7, Dyn(x, TyI32))
      case AtomData$StoryQuestionsAtom(x) ⇒ (-8, Dyn(x, TyI32))
      case AtomData$QAndAAtom(x)          ⇒ (-9, Dyn(x, TyI32))
      case AtomData$GuideAtom(x)          ⇒ (-10, Dyn(x, TyI32))
      case AtomData$ProfileAtom(x)        ⇒ (-11, Dyn(x, TyI32))
      case AtomData$TimelineAtom(x)       ⇒ (-12, Dyn(x, TyI32))
    }

    def decode = {
      case (-1, x)  ⇒ Dynamic.cast(x, TyI32).map(AtomData$QuizAtom)
      case (-2, x)  ⇒ Dynamic.cast(x, TyI32).map(AtomData$MediaAtom)
      case (-3, x)  ⇒ Dynamic.cast(x, TyI32).map(AtomData$ExplainerAtom)
      case (-4, x)  ⇒ Dynamic.cast(x, TyI32).map(AtomData$CTAAtom)
      case (-5, x)  ⇒ Dynamic.cast(x, TyI32).map(AtomData$InteractiveAtom)
      case (-6, x)  ⇒ Dynamic.cast(x, TyI32).map(AtomData$ReviewAtom)
      case (-7, x)  ⇒ Dynamic.cast(x, TyI32).map(AtomData$RecipeAtom)
      case (-8, x)  ⇒ Dynamic.cast(x, TyI32).map(AtomData$StoryQuestionsAtom)
      case (-9, x)  ⇒ Dynamic.cast(x, TyI32).map(AtomData$QAndAAtom)
      case (-10, x) ⇒ Dynamic.cast(x, TyI32).map(AtomData$GuideAtom)
      case (-11, x) ⇒ Dynamic.cast(x, TyI32).map(AtomData$ProfileAtom)
      case (-12, x) ⇒ Dynamic.cast(x, TyI32).map(AtomData$TimelineAtom)
      case _        ⇒ None
    }
  }
}