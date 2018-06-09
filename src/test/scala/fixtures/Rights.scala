package test.fixtures

import humbug._
import humbug.codecs._

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

case class Rights(
    syndicatable:          Boolean,
    subscriptionDatabases: Option[Boolean] = Some(false),
    developerCommunity:    Option[Boolean] = None
) extends TStruct

object Rights {
  import Arbitrary._

  implicit val arbRights: Arbitrary[Rights] = Arbitrary {
    for {
      s ← arbitrary[Boolean]
      sd ← arbitrary[Option[Boolean]]
      dc ← arbitrary[Option[Boolean]]
    } yield Rights(s, sd, dc)
  }

  implicit val codec: TStructCodec[Rights] = new TStructCodec[Rights] {
    final val fieldIds = List(1, 2, -1)

    final val defaults = Map(
      2.toShort -> Dyn(Some(false), TyOpt(TyBool)),
      -1.toShort -> Dyn(None, TyOpt(TyBool))
    )

    def encode = r ⇒ defaults ++ r.leftovers ++ Map(
      1.toShort -> Dyn(r.syndicatable, TyBool),
      2.toShort -> Dyn(r.subscriptionDatabases, TyOpt(TyBool)),
      -1.toShort -> Dyn(r.developerCommunity, TyOpt(TyBool))
    )

    def decode = m ⇒ for {
      syndicatable ← m.get(1).orElse(defaults.get(1)).flatMap(Dynamic.cast(_, TyBool))
      subscriptionDatabases ← m.get(2).orElse(defaults.get(2)).flatMap(Dynamic.cast(_, TyOpt(TyBool)))
      developerCommunity ← m.get(-1).orElse(defaults.get(-1)).flatMap(Dynamic.cast(_, TyOpt(TyBool)))
    } yield new Rights(syndicatable, subscriptionDatabases, developerCommunity) {
      override def leftovers = m -- fieldIds
    }
  }
}