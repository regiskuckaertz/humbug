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

In Scala world, we don't care about those numbers at all; they're only necessary during serialization/deserialization. The user should never see them anywhere near their code. The only acceptable representation should be as simple as that:

```scala
sealed trait ContentType extends TEnum
case object ARTICLE     extends ContentType
case object LIVEBLOG    extends ContentType
case object GALLERY     extends ContentType
case object INTERACTIVE extends ContentType
case object PICTURE     extends ContentType
case object VIDEO       extends ContentType
case object CROSSWORD   extends ContentType
case object AUDIO       extends ContentType
```

The companion object will hold the type-specific logic for encoding and decoding:

```scala
object ContentType extends TEnumCodec[ContentType] {

  override def encode = {
    case ARTICLE     => 0
    case LIVEBLOG    => 1
    case GALLERY     => 2
    case INTERACTIVE => 3
    case PICTURE     => 4
    case VIDEO       => 5
    case CROSSWORD   => 6
    case AUDIO       => 7
  }

  override def decode = {
    case 0 => ARTICLE
    case 1 => LIVEBLOG
    case 2 => GALLERY
    case 3 => INTERACTIVE
    case 4 => PICTURE
    case 5 => VIDEO
    case 6 => CROSSWORD
    case 7 => AUDIO
  }
}
```

Let's define `TEnumCodec` right here:

```scala
trait TEnumCodec[A] {
  def encode: Function1[A, Int]
  def decode: PartialFunction[Int, A]
}
```

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

Note how the optional `developerCommunity` field doesn't have a default value. We must assign it `None` by default, as application users will naturally expect it that way when they read the Thrift definition.

Again the companion object will hold type-specific logic. But what does that logic look like? A struct is itself defined by its fields, so we need a way to uniquely provide field-specific logic, i.e.

- the field id, a signed integer
- the field type
- the field default value, if any

Intuitively, we need a set of tuples `(Int, Type, Option[Type])`. Thinking one step further, field identifiers determine the type and default value, forming a partial function from `Int` to `(Type, Option[Type])`. Shapeless provides a convenient way of representing these partial functions in the form of heterogeneous maps. We will use that encoding for our first try:

```scala
class KVRelation[K, V]
type TFieldCodec[A] = KVRelation[Int, A]

object Rights extends TStructCodec[Rights] {
  implicit val r1 = new TFieldCodec[Boolean]
  implicit val r2 = new TFieldCodec[Option[Boolean]]

  override val defaults = HMap[KVRelation](
    2  -> (Some(false): Option[Boolean]),
    -1 -> (None: Option[Boolean])
  )
  
  override def encode = x => HMap[KVRelation](
    1  -> x.syndicatable,
    2  -> x.subscriptionDatabases,
    -1 -> x.developerCommunity
  )

  override def decode = m => for {
    syndicatable <- m.get(1).orElse(defaults.get(1))
    subscriptionDatabases <- m.get(2).orElse(defaults.get(2))
    developerCommunity <- m.get(-1).orElse(defaults.get(-1))
  } yield Rights(syndicatable, subscriptionDatabases, developerCommunity)
}
```

Ok let's unpack this:

1. We start by defining the relation between key types and values types. `HMap` requires a binary type and a set of implicit values to witness these relations
2. We go on by defining a map of default values. Also notice how we annotate each value with the expected type: that's because `HMap` is invariant in its typing relation.
3. Our encoding function will produce a map of each field with its corresponding identifier
4. Finally, our decoding function produces a value if all its fields get one (which explains why optional fields with no default value must have a `None` assigned to them by default)

Waaaaait a minute. How can the heterogenous map figure out the type of a value if all the keys share the same type? Houston, we've got a problem. Indeed, with the above the compiler will complain that there are several implicit values to resolve the equation `KVRelation[Int, ?]`. Back to the drawing board. It would be nice if we could assign each integer value its own type. Hold on, isn't that what singleton types are? Yikes! It turns out Shapeless provides everything we need to narrow down the type of an integer value to its singleton type.

Let's give it another shot:

```scala
class TFieldCodec[K, V]

object Rights extends TStructCodec[Rights] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(-1)

  implicit val r1 = new TFieldCodec[w1.T, Boolean]
  implicit val r2 = new TFieldCodec[w2.T, Option[Boolean]]
  implicit val r3 = new TFieldCodec[w3.T, Option[Boolean]]

  override val defaults = HMap[TFieldCodec](
    w2.value -> (Some(false): Option[Boolean]),
    w3.value -> (None: Option[Boolean])
  )
  
  override def encode = x => HMap[TFieldCodec](
    w1.value -> x.syndicatable,
    w2.value -> x.subscriptionDatabases,
    w3.value -> x.developerCommunity
  )

  override def decode = m => for {
    syndicatable <- m.get(w1.value).orElse(defaults.get(w1.value))
    subscriptionDatabases <- m.get(w2.value).orElse(defaults.get(w2.value))
    developerCommunity <- m.get(w3.value).orElse(defaults.get(w3.value))
  } yield Rights(syndicatable, subscriptionDatabases, developerCommunity)
}
```

NB: In a future iteration, I will get rid of all that boilerplate by using `Generic`s.

A final note before we move one: notice how we choose to encode all the fields, even those which might hold their respective default value? It will be the responsibility of the serializer to choose whether or not to wire those values on the output stream.

We have defined `TFieldCodec` but not `TStructCodec` yet:

```scala
trait TStructCodec[A] {
  def defaults: HMap[KVRelation]
  def encode: A => HMap[KVRelation]
  def decode: HMap[KVRelation] => Option[A]
}
```

## Unions

Under Thrift semantics, unions are like a cross between enums and structs. In Scala, this is again expressed in the form of a sealed trait. For instance,

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

becomes, as one would expect

```scala
sealed trait AtomData extends TStruct
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

Note that we tag unions as structs, because their encoding looks is similar from the outside. We will reuse the `HMap` abstraction, even though there will only ever be a single value (I've skipped the singleton type kung-fu, you can mentally fill in the blanks):

```scala
object AtomData extends TStructCodec[AtomData] {
  implicit val r1 = new TFieldCodec[QuizAtom]
  implicit val r2 = new TFieldCodec[Media]
  implicit val r3 = new TFieldCodec[Explainer]
  implicit val r4 = new TFieldCodec[Cta]
  implicit val r5 = new TFieldCodec[Interactive]
  implicit val r6 = new TFieldCodec[Review]
  implicit val r7 = new TFieldCodec[Recipe]
  implicit val r8 = new TFieldCodec[Storyquestions]
  implicit val r9 = new TFieldCodec[Qanda]
  implicit val r10 = new TFieldCodec[Guide]
  implicit val r11 = new TFieldCodec[Profile]
  implicit val r12 = new TFieldCodec[Timeline]

  def encode = {
    case Quiz(x)            => HMap[KVRelation](1  -> x)
    case Media(x)           => HMap[KVRelation](-1 -> x)
    case Explainer(x)       => HMap[KVRelation](-2 -> x)
    case Cta(x)             => HMap[KVRelation](5  -> x)
    case Interactive(x)     => HMap[KVRelation](6  -> x)
    case Review(x)          => HMap[KVRelation](7  -> x)
    case Recipe(x)          => HMap[KVRelation](-3 -> x)
    case Storyquestions(x)  => HMap[KVRelation](9  -> x)
    case Qanda(x)           => HMap[KVRelation](10 -> x)
    case Guide(x)           => HMap[KVRelation](11 -> x)
    case Profile(x)         => HMap[KVRelation](12 -> x)
    case Timeline(x)        => HMap[KVRelation](13 -> x)
  }

  def decode = m =>
    m get(1)  map(Quiz(_)) orElse
    m get(-1) map(Media(_)) orElse
    m get(-2) map(Explainer(_)) orElse
    m get(5)  map(Cta(_)) orElse
    m get(6)  map(Interactive(_)) orElse
    m get(7)  map(Review(_)) orElse
    m get(-3) map(Recipe(_)) orElse
    m get(9)  map(Storyquestions(_)) orElse
    m get(10) map(Qanda(_)) orElse
    m get(11) map(Guide(_)) orElse
    m get(12) map(Profile(_)) orElse
    m get(13) map(Timeline(_))
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

We still need to tag those values because the underlying values must be lifted during deserialization. We also tag with `TTypeDef`, that will come in handy later:

```scala
trait TTypeDef extends Any
```

The companion object provides trivial wrapping/unwrapping logic:

```scala
trait TTypeDefCodec[A, B] {
  def encode: A => B
  def decode: B => A
}

object Datetime extends TTypeDefCodec[Datetime, Long] {
  override def encode = _.value
  override def decode = new Datetime(_)
}
```

## Namespaces

Last but not least. In Thrift, you define namespaces in a thrift document, and then import definitions from another document by file name. This asymmetry is a small challenge, but one we can overcome by taking a step back and looking at the parsing algorithm. First, the easy part: namespaces.

We only care about Java namespaces, any other one will be silently ignored. Informally, this:

```thrift
namespace java com.gu.contentapi.client.model.v1
```

will be turned into a Scala package:

```scala
package com.gu.contentapi.client.model.v1
```

No surprise here. Ok, now to imports. An import statement looks like this:

```thrift
include "atoms/storyquestions.thrift"
```

and then later on in the same document, one references types defined in the above document using the filename as a prefix, e.g.:

```thrift
union AtomData {
  storyquestions.StoryQuestionAtom storyquestions
}
```

Our compiler has to figure out (1) how to turn that include statement into a Scala `import` and (2) that the included document actually produces a type called `StoryQuestionsAtom` (Actually, the compiler will "typecheck" thrift types by making sure all types are defined, no matter if they are coming from a separate document or not).

Formally, a Thrift document yields the tuple _σ_ = (_p_, _I_, _C_) where

- _p_ is a package name
- _I_ is a set of include statements { _i1_, ..., _in_ } where each _ii_ stands for a path to another Thrift document
- _C_ is a set of types { _c1_, ..., _cn_ } where each _ci_ stands for an enum, a struct, a typedef or a union

Our compiler produces a set _Σ_ = { _σ1_, ..., _σn_ }. We will use the same strategy many compilers for languages with type polymorphism use, which is to use _type variables_ (in our case also _import variables_) and record a set of _constraints_ that must be unified for the compiler to produce a sensible output. A constraint is an equation X = Y where each side is either a variable or a type (that may include other variables).

Every time our compiler comes across an import statement, it will

- create a fresh import variable _I_
- swap it in place of the statement
- record a constraint _I_ = _σ_.p where _σ_ ∈ _Σ_

Every time our compiler comes across a type prefixed with the filename of an import, it will

- create a fresh type variable _C_
- swap it in place of the type
- record a constraint _C_ = _σ_.ci where _σ_ ∈ _Σ_ and ci ∈ _σ_.C

The set of constraints yields a substitution function that will unify our thrift term into a closed term (free of variables). If it succeeds, great! We can move on to generating the Scala output. If it can't, boo it sucks: there's a mistake and the compiler must inform the user that shit happened. If we do things correctly, we can even be super specific in the import statement, i.e. only include those things that are actually being used in the document.

Say the document shared.thrift contains

```thrift
package scala io.github.regiskuckaertz.shared

typedef Datetime i64
```

and then another document contains

```thrift
include "shared.thrift"

...

struct ChangeRecord {
  int64 userId
  shared.Datetime when
}
```

These together yield

```scala
import io.github.regiskuckaertz.shared.Datetime

case class ChangeRecord {
  userId: Long,
  when: Datetime
}
```

## Consts

A constant in a Thrift document, such as

```thrift
package * io.github.regiskuckaertz.humbug.sample

const i32 MAX_CONNECTIONS = 256
````

is turned into a value in the package object corresponding to that document:

```scala
package io.github.regiskuckaertz.humbug

package object sample {
  val MAX_CONNECTIONS: Int = 256
```
