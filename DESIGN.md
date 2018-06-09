## Enums

Let us start with a simple enumeration:

```thrift
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

The Scala language provides an `Enumeration` class, the translation is straightforward:

```scala
object ContentType extends Enumeration {
  type ContentType = Value
  val ARTICLE = Val(0)
  val LIVEBLOG = Val(1)
  val GALLERY = Val(2)
  val INTERACTIVE = Val(3)
  val PICTURE = Val(4)
  val VIDEO = Val(5)
  val CROSSWORD = Val(6)
  val AUDIO = Val(7)
}
```

The companion object will hold the type-specific logic for encoding and decoding, but here it is trivial:

```scala
object ContentType {
  implicit val codec = new TEnumCodec[ContentType] {
    override def encode = _.id
    override def decode = ContentType(_)
  }
}
```

Let's define `TEnumCodec` right here:

```scala
trait TEnumCodec[A] {
  def encode: A => Int
  def decode: Int => A
}
```

Notice how the encoding above does not favour any particular serialisation protocol. Thrift has many, so it is a good idea to delegate that to another layer. This theme will apply across all the other encodings: the codec for any type `A` will just provide the necessary logic to read/write to/from an encoding that can then be serialisation in whatever fashion.

## Structs

In Thrift, type designers may define field identifiers---note that fields without identifiers are implicitly assigned negative identifiers in decreasing order, starting from `-1`---, but users of these types should never care about those. So the following structure:

```thrift
struct Rights {
    1: required bool syndicatable
    2: optional bool subscriptionDatabases = false
    optional bool developerCommunity
}
```

Should just compile to:

```scala
case class Rights(
  syndicatable         : Boolean,
  subscriptionDatabases: Option[Boolean] = Some(false),
  developerCommunity   : Option[Boolean] = None
) extends TStruct
```

Note how the optional `developerCommunity` field doesn't have a default value. We must assign it `None` by default, as application users will naturally expect it that way when they read the Thrift definition. What is `TStruct`? The cost of backward- and forward-compatibility. When deserialising a value, we must be aware of the possibility that it has been produced by an entity using a different version of the Thrift model. We cannot just discard unrecognised fields and call it a day; if the value is to be passed around to another entity, we must make sure these values are sent along the way too:

```scala
type Fields = Map[FieldID, Dynamic]
abstract class TStruct protected {
  def leftovers: Fields
}
```

Again the companion object will hold type-specific logic. But what does that logic look like? A class is isomorphic to a tuple of the same order and types, but tuples are not very handy. Moreover, for each field we not only need its value but also a representation of its type and its identifier. For the former, we come up with a data type to represent values along with their type:

```scala
sealed abstract class Dynamic
final case class Dyn[A](value: A, typ: Type[A]) extends Dynamic
```

This is useful when we need to read a value from the outside world and prove to the compiler it has a particular type. For this, we provide a casting operator:

```scala
def cast[A](dyn: Dynamic, typ: Type[A]): Option[A]
```

Ok, back to business. From the above, encoding/decoding at the companion object level is straightforward:

```scala
object Rights {
  implicit val codec = new TStructCodec[Rights] {
    final val fieldIds = List(1, 2, -1)

    final val defaults = Map(
      2  -> Dyn(Some(false), TyOpt[TyBool]]),
      -1 -> Dyn(None, TyOpt[TyBool]])
    )
  
    override def encode = x => defaults ++ x.leftovers ++ Map(
      1  -> Dyn(x.syndicatable, TyBool]),
      2  -> Dyn(x.subscriptionDatabases, TyOpt[TyBool]),
      -1 -> Dyn(x.developerCommunity, TyOpt[TyBool])
    )

    override def decode = m => for {
      syndicatable <- m.get(1).orElse(defaults.get(1)).flatMap(cast(_, TyBool))
      subscriptionDatabases <- m.get(2).orElse(defaults.get(2)).flatMap(cast(_, TyOpt[TyBool]))
      developerCommunity <- m.get(-1).orElse(defaults.get(-1)).flatMap(cast(_, TyOpt[TyBool]))
    } yield Rights(syndicatable, subscriptionDatabases, developerCommunity) {
      override def leftovers = m -- fieldIds
    }
  }
}
```

We have defined `TFieldCodec` but not `TStructCodec` yet:

```scala
abstract class TStructCodec[A] {
  def fieldIds: List[FieldID]
  def defaults: Fields
  def encode: A => Fields
  def decode: Fields => Option[A]
}
```

## Unions

Under Thrift semantics, unions are like a cross between enums and structs. In Scala, this is expressed in the form of an algebraic data type. For instance,

```thrift
union AtomData {
  1: QuizAtom quiz
  MediaAtom media
  ExplainerAtom explainer
  5: CTAAtom cta
  6: InteractiveAtom interactive
  7: ReviewAtom review
  RecipeAtom recipe
  9: StoryQuestionsAtom storyquestions
  10: QAndAAtom qanda
  11: GuideAtom guide
  12: ProfileAtom profile
  13: TimelineAtom timeline
}
```

For the sake of simplicity, let's assume all these types are synonyms for integers. In Scala, it naturally becomes:

```scala
type QuizAtom = Int
type MediaAtom = Int
type ExplainerAtom = Int
type CTAAtom = Int
type InteractiveAtom = Int
type ReviewAtom = Int
type RecipeAtom = Int
type StoryQuestionsAtom = Int
type QAndAAtom = Int
type GuideAtom = Int
type ProfileAtom = Int
type TimelineAtom = Int
sealed trait AtomData extends TUnion
case class AtomData$QuizAtom(quiz: QuizAtom) extends AtomData
case class AtomData$MediaAtom(media: MediaAtom) extends AtomData
case class AtomData$ExplainerAtom(explainer: ExplainerAtom) extends AtomData
case class AtomData$CTAAtom(cta: CTAAtom) extends AtomData
case class AtomData$InteractiveAtom(interactive: InteractiveAtom) extends AtomData
case class AtomData$ReviewAtom(review: ReviewAtom) extends AtomData
case class AtomData$RecipeAtom(recipe: RecipeAtom) extends AtomData
case class AtomData$StoryQuestionsAtom(storyquestions: StoryQuestionsAtom) extends AtomData
case class AtomData$QAndAAtom(qanda: QAndAAtom) extends AtomData
case class AtomData$GuideAtom(guide: GuideAtom) extends AtomData
case class AtomData$ProfileAtom(profile: ProfileAtom) extends AtomData
case class AtomData$TimelineAtom(timeline: TimelineAtom) extends AtomData
```

Unions are very similar to structs, except that only one field is ever marshalled/unmarshalled. Therefore, a tuple is more than enough and we can get away without using a `Map`:

```scala
trait TUnionCodec[A] {
  def encode: A => (FieldID, Dynamic)
  def decode: (FieldID, Dynamic) => Option[A]
}

object AtomData {
  implicit val codec = new TUnionCodec[AtomData] {
    def encode = {
      case AtomData$QuizAtom(x)           => (1, Dyn(x, TyI32))
      case AtomData$MediaAtom(x)          => (-1, Dyn(x, TyI32))
      case AtomData$ExplainerAtom(x)      => (-2, Dyn(x, TyI32))
      case AtomData$CTAAtom(x)            => (5, Dyn(x, TyI32))
      case AtomData$InteractiveAtom(x)    => (6, Dyn(x, TyI32))
      case AtomData$ReviewAtom(x)         => (7, Dyn(x, TyI32))
      case AtomData$RecipeAtom(x)         => (-3, Dyn(x, TyI32))
      case AtomData$StoryQuestionsAtom(x) => (9, Dyn(x, TyI32))
      case AtomData$QAndAAtom(x)          => (10, Dyn(x, TyI32))
      case AtomData$GuideAtom(x)          => (11, Dyn(x, TyI32))
      case AtomData$ProfileAtom(x)        => (12, Dyn(x, TyI32))
      case AtomData$TimelineAtom(x)       => (13, Dyn(x, TyI32))
    }

    def decode = {
      case (1, x)  => Dynamic.cast(x, TyI32).map(AtomData$QuizAtom)
      case (-1, x) => Dynamic.cast(x, TyI32).map(AtomData$MediaAtom)
      case (-2, x) => Dynamic.cast(x, TyI32).map(AtomData$ExplainerAtom)
      case (5, x)  => Dynamic.cast(x, TyI32).map(AtomData$CTAAtom)
      case (6, x)  => Dynamic.cast(x, TyI32).map(AtomData$InteractiveAtom)
      case (7, x)  => Dynamic.cast(x, TyI32).map(AtomData$ReviewAtom)
      case (-3, x) => Dynamic.cast(x, TyI32).map(AtomData$RecipeAtom)
      case (9, x)  => Dynamic.cast(x, TyI32).map(AtomData$StoryQuestionsAtom)
      case (10, x) => Dynamic.cast(x, TyI32).map(AtomData$QAndAAtom)
      case (11, x) => Dynamic.cast(x, TyI32).map(AtomData$GuideAtom)
      case (12, x) => Dynamic.cast(x, TyI32).map(AtomData$ProfileAtom)
      case (13, x) => Dynamic.cast(x, TyI32).map(AtomData$TimelineAtom)
      case _       => None
    }
  }
}
```

## Typedefs

Typedefs provide a convenient type aliasing mechanism, as provided by many languages:

```thrift
typedef Datetime i64
```

With Scala, we can provide an additional layer of type safety by using _value classes_:

```scala
class Datetime(val value: Long) extends AnyVal with TTypeDef
```

We still need to tag those values because the underlying values must be lifted during deserialization.

```scala
trait TTypeDef extends Any
```

The codec witnesses an isomorphism between the type alias and its underlying value, as it should. We just need to keep track of the type of that underlying value and its representation:

```scala
trait TTypeDefCodec[A] {
  type Rep
  def typeRep: Type[Rep]
  def encode: A => Rep
  def decode: Rep => A
}

object Datetime {
  implicit val codec = new TTypeDefCodec[Datetime] {
    type Rep = Long
    val typeRep = TyI64
    def encode = _.value
    def decode = new Datetime(_)
  }
}
```

## Consts

A constant in a Thrift document, such as

```thrift
package * io.github.regiskuckaertz.humbug.test.fixtures

const i32 MAX_CONNECTIONS = 256
````

is turned into a value in the package object corresponding to that document:

```scala
package io.github.regiskuckaertz.humbug.test

package object fixtures {
  val MAX_CONNECTIONS: Int = 256
}
```
