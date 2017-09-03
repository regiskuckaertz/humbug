package humbug
package codec
package compact

import shapeless._, shapeless.nat._, shapeless.ops.nat._

trait CompactWitness[A] {
  type N <: Nat

  def value: Int
}

object CompactWitness {
  def mkCompactWitness[A, N0 <: Nat](implicit toInt: ToInt[N0]) =
    new CompactWitness[A] {
      type N = N0

      val value: Int = toInt()
    }

  implicit val bow = mkCompactWitness[Boolean, _2]
  implicit val byw = mkCompactWitness[Byte, _3]
  implicit val dow = mkCompactWitness[Double, _4]
  implicit val sow = mkCompactWitness[Short, _6]
  implicit val inw = mkCompactWitness[Int, _8]
  implicit val low = mkCompactWitness[Long, _10]
  implicit val stw = mkCompactWitness[String, _11]
  implicit val vbw = mkCompactWitness[Vector[Byte], _11]
  implicit val tsw = mkCompactWitness[ThriftStruct, _12]
  implicit val maw = mkCompactWitness[Map[_, _], _13]
  implicit val sew = mkCompactWitness[Set[_], _14]
  implicit val liw = mkCompactWitness[List[_], _15]
}
