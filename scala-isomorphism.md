```
enum ContentType {
    ARTICLE = 0,
    LIVEBLOG = 1,
    GALLERY = 2,
    INTERACTIVE = 3,
    PICTURE = 4,
    VIDEO = 5,
    CROSSWORD = 6,
    AUDIO = 7
}
```

compiles to

```
sealed abstract class ContentType extends ThriftEnum
case object ARTICLE     extends ContentType
case object LIVEBLOG    extends ContentType
case object GALLERY     extends ContentType
case object INTERACTIVE extends ContentType
case object PICTURE     extends ContentType
case object VIDEO       extends ContentType
case object CROSSWORD   extends ContentType
case object AUDIO       extends ContentType

object ContentType {
  implicit codec = new ThriftEnumGeneric[ContentType] {
    def from: Int => Option[ContentType] = {
      case 0 => Some(ARTICLE)
      case 1 => Some(LIVEBLOG)
      case 2 => Some(GALLERY)
      case 3 => Some(INTERACTIVE)
      case 4 => Some(PICTURE)
      case 5 => Some(VIDEO)
      case 6 => Some(CROSSWORD)
      case 7 => Some(AUDIO)
      case _ => None
    }

    def to: ContentType => Int = {
      case ARTICLE     => 0
      case LIVEBLOG    => 1
      case GALLERY     => 2
      case INTERACTIVE => 3
      case PICTURE     => 4
      case VIDEO       => 5
      case CROSSWORD   => 6
      case AUDIO       => 7
    }
  }
}
```

---
```
struct Rights {
    1: optional bool syndicatable = false
    2: optional bool subscriptionDatabases = false
    optional bool developerCommunity = false
}
```

compiles to

```
case class Rights(
  syndicatable         : Option[Boolean] = Some(false),
  subscriptionDatabases: Option[Boolean] = Some(false),
  developerCommunity   : Option[Boolean] = Some(false)
) extends ThriftStruct
```

the translation is literal for base types, i.e. an optional field of type `A`
becomes an `Option[A]` and a required field of type `A` becomes an `A`. Container
types embody the notion of emptiness so there is no need to wrap them in an
`Option`:

```
  optional list<i32> ints
```

becomes

```
  ints: List[Int] = List[Int].empty
```

Now the Thrift protocol doesn't care about field names but rather field IDs.
We need a way to encode them while keeping them separate from the datatype.
The field ID is just an Int, and it is easy to build an HList of those Ints
that we can zip together with the HList corresponding to the case class. This
generic representation will be referred to as PositionedGeneric[Rights] which
is isomorphic to Generic[Rights], only that each element of the former is a
FieldType[K, V], where the K is the singleton Int corresponding to the field ID.
To materialize this HList, we will use an intermediate typeclass Indices, holding
an HList of singleton Ints corresponding to Rights. That is the only piece of
metadata the compiler needs to produce. In our example:

object Rights {
  import humbug.internal.Indices

  implicit val indices: Indices[Rights] = new Indices[Rights] {
    type Repr = Int :: Int :: Int :: HNil

    val keys = 1 :: 2 :: -1 :: HNil
  }
}

---
```
namespace java com.gu.contentapi.client.model.v1
```

becomes

```
package com.gu.contentapi.client.model.v1
```

---

```
include "story_package_article.thrift"
```

becomes

```
import <resolve package name by parsing thrift file and extracting the correct namespace>
```

e.g.

```
import com.gu.contentapi.storypackage
```

- this process can be lazy, to avoid the cost of parsing files when they are unused
- the tip of the package will be spliced in replacement of the thrift definition name, i.e.

```
  story_package_article.Story
```

becomes

```
  storypackage.Story
```

---

```
union AtomData {
  1: quiz.QuizAtom quiz
  media.MediaAtom media
  explainer.ExplainerAtom explainer
  5: cta.CTAAtom cta
  6: interactive.InteractiveAtom interactive
  7: review.ReviewAtom review
  recipe.RecipeAtom recipe
  9: storyquestions.StoryQuestionsAtom storyquestions
  10: qanda.QAndAAtom qanda
  11: guide.GuideAtom guide
  12: profile.ProfileAtom profile
  13: timeline.TimelineAtom timeline
}
```

becomes

```
sealed abstract class AtomData[A] extends ThriftStruct {
  def quiz: Some[A] = None
  def media: Some[A] = None
  def explainer: Some[A] = None
  def cta: Some[A] = None
  def interactive: Some[A] = None
  def review: Some[A] = None
  def recipe: Some[A] = None
  def storyquestions: Some[A] = None
  def qanda: Some[A] = None
  def guide: Some[A] = None
  def profile: Some[A] = None
  def timeline: Some[A] = None
}
case class Quiz           extends AtomData[quiz.QuizAtom with 1.type] {
  override def quiz = Some(this)
}
case class Media          extends AtomData[media.MediaAtom with -1.type] {
  override def media = Some(this)
}
case class Explainer      extends AtomData[explainer.ExplainerAtom with -2.type] {
  override def explainer = Some(this)
}
case class Cta            extends AtomData[cta.CTAAtom with 5.type] {
  override def cta = Some(this)
}
case class Interactive    extends AtomData[interactive.InteractiveAtom with 6.type] {
  override def interactive = Some(this)
}
case class Review         extends AtomData[review.ReviewAtom with 7.type] {
  override def review = Some(this)
}
case class Recipe         extends AtomData[recipe.RecipeAtom with -3.type] {
  override def recipe = Some(this)
}
case class Storyquestions extends AtomData[storyquestions.StoryQuestionsAtom with 9.type] {
  override def storyquestions = Some(this)
}
case class Qanda          extends AtomData[qanda.QAndAAtom with 10.type] {
  override def qanda = Some(this)
}
case class Guide          extends AtomData[guide.GuideAtom with 11.type] {
  override def guide = Some(this)
}
case class Profile        extends AtomData[profile.ProfileAtom with 12.type] {
  override def profile = Some(this)
}
case class Timeline       extends AtomData[timeline.TimelineAtom with 13.type] {
  override def timeline = Some(this)
}
```

---

```
typedef Datetime string
```

becomes

```
case class Datetime(value: String) extends AnyVal
```
types embody the notion of emptiness so there is no need to wrap them in an
