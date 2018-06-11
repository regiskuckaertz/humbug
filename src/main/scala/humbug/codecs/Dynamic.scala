package humbug
package codecs

sealed abstract class Type[T](val typeId: Int) extends Product with Serializable
final case object TyBool extends Type[Boolean](2)
final case object TyByte extends Type[Byte](3)
final case object TyDouble extends Type[Double](4)
final case object TyI16 extends Type[Short](6)
final case object TyI32 extends Type[Int](8)
final case object TyI64 extends Type[Long](10)
final case object TyString extends Type[String](11)
final case object TyStruct extends Type[Map[Short, Dynamic]](12)
final case object TyUnion extends Type[(Short, Dynamic)](12)
final case class TyTypeDef[A](ta: Type[A]) extends Type[A](ta.typeId)
final case class TyList[A](ta: Type[A]) extends Type[List[A]](15)
final case class TySet[A](ta: Type[A]) extends Type[Set[A]](14)
final case class TyOpt[A](ta: Type[A]) extends Type[Option[A]](ta.typeId)
final case class TyMap[K, V](tk: Type[K], tv: Type[V]) extends Type[Map[K, V]](12)
final case object TyDyn extends Type[Dynamic](0)

sealed abstract class Dynamic extends Product with Serializable
final case class Dyn[A](value: A, typ: Type[A]) extends Dynamic

object Dynamic {
  def tequal[A, B](ta: Type[A], tb: Type[B]): Option[A ⇒ B] = (ta, tb) match {
    case (TyBool, TyBool)               ⇒ Some(identity(_))
    case (TyByte, TyByte)               ⇒ Some(identity(_))
    case (TyDouble, TyDouble)           ⇒ Some(identity(_))
    case (TyI16, TyI16)                 ⇒ Some(identity(_))
    case (TyI32, TyI32)                 ⇒ Some(identity(_))
    case (TyI64, TyI64)                 ⇒ Some(identity(_))
    case (TyString, TyString)           ⇒ Some(identity[String](_))
    case (TyStruct, TyStruct)           ⇒ Some(identity(_))
    case (TyUnion, TyUnion)             ⇒ Some(identity(_))
    case (TyTypeDef(ta), TyTypeDef(tb)) ⇒ tequal(ta, tb).map { cst ⇒ la ⇒ cst(la) }
    case (TyList(ta), TyList(tb))       ⇒ tequal(ta, tb).map { cst ⇒ la ⇒ la.map(cst) }
    case (TySet(ta), TySet(tb))         ⇒ tequal(ta, tb).map { cst ⇒ sa: Set[_] ⇒ sa.map(cst) }
    case (TyOpt(ta), TyOpt(tb))         ⇒ tequal(ta, tb).map { cst ⇒ oa: Option[_] ⇒ oa.map(cst) }
    case (TyDyn, TyDyn)                 ⇒ Some(identity(_))
    case (TyMap(tka, tva), TyMap(tkb, tvb)) ⇒ for {
      castk ← tequal(tka, tkb)
      castv ← tequal(tva, tvb)
    } yield { mkv: Map[_, _] ⇒ mkv.map { case (k, v) ⇒ castk(k) -> castv(v) } }
    case _ ⇒ None
  }

  def cast[A](dyn: Dynamic, ta: Type[A]): Option[A] = dyn match {
    case Dyn(value, typ) ⇒ tequal(typ, ta).map { f ⇒ f(value) }
  }
}