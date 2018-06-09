package test.fixtures

import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.option._

import humbug._
import humbug.codecs._

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

case class Content(
    id:          String,
    `type`:      ContentType.ContentType,
    sectionId:   String,
    sectionName: Option[String],
    //5: optional CapiDateTime webPublicationDate
    references: List[Reference],
    rights:     Option[Rights]
) extends TStruct

object Content {
  import Arbitrary._

  implicit val arbRights: Arbitrary[Content] = Arbitrary {
    for {
      id ← arbitrary[String]
      `type` ← arbitrary[ContentType.ContentType]
      sectionId ← arbitrary[String]
      sectionName ← arbitrary[Option[String]]
      references ← arbitrary[List[Reference]]
      rights ← arbitrary[Option[Rights]]
    } yield Content(id, `type`, sectionId, sectionName, references, rights)
  }

  implicit def codec(implicit E: TStructCodec[Reference], E2: TEnumCodec[ContentType.ContentType], E3: TStructCodec[Rights]) = new TStructCodec[Content] {
    final val fieldIds = List(1, 2, 3, 4, 10, 15)

    final val defaults = Map(
      2.toShort -> Dyn(E2.encode(ContentType.ARTICLE), TyI32),
      10.toShort -> Dyn(Nil, TyList(TyStruct))
    )

    def encode = v ⇒ defaults ++ v.leftovers ++ Map(
      1.toShort -> Dyn(v.id, TyString),
      2.toShort -> Dyn(E2.encode(v.`type`), TyI32),
      3.toShort -> Dyn(v.sectionId, TyString),
      4.toShort -> Dyn(v.sectionName, TyOpt(TyString)),
      10.toShort -> Dyn(v.references.map(E.encode), TyList(TyStruct)),
      15.toShort -> Dyn(v.rights.map(E3.encode), TyOpt(TyStruct))
    )

    def decode = m ⇒ for {
      id ← m.get(1).orElse(defaults.get(1)).flatMap(Dynamic.cast(_, TyString))
      `type` ← m.get(2).orElse(defaults.get(2)).flatMap(Dynamic.cast(_, TyI32)).flatMap(E2.decode)
      sectionId ← m.get(3).orElse(defaults.get(3)).flatMap(Dynamic.cast(_, TyString))
      sectionName ← m.get(4).orElse(defaults.get(4)).flatMap(Dynamic.cast(_, TyOpt(TyString)))
      references ← m.get(10).orElse(defaults.get(10)).flatMap(Dynamic.cast(_, TyList(TyStruct))).flatMap(_.traverse(E.decode))
      rights ← m.get(15).orElse(defaults.get(15)).flatMap(Dynamic.cast(_, TyOpt(TyStruct))).flatMap(_.traverse(E3.decode))
    } yield new Content(id, `type`, sectionId, sectionName, references, rights) {
      override def leftovers = m -- fieldIds
    }
  }
}