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
becomes an `Option[A]` and a required field of type `A` becomes an `A`.

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

becomes, as one would expect

```
sealed trait AtomData extends ThriftUnion
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
```

---

```
typedef Datetime string
```

becomes

```
case class Datetime(value: String) extends AnyVal
```

