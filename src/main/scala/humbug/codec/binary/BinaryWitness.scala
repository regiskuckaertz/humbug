package humbug
package codec
package binary

import shapeless._, shapeless.nat._, shapeless.ops.nat._

trait BinaryWitness[A] {
  type N <: Nat

  def value: Int
}

object BinaryWitness {
  def mkBinaryWitness[A, N0 <: Nat](implicit toInt: ToInt[N0]) =
    new BinaryWitness[A] {
      type N = N0

      val value: Int = toInt()
    }

  implicit val bow = mkBinaryWitness[Boolean, _2]
  implicit val byw = mkBinaryWitness[Byte, _3]
  implicit val dow = mkBinaryWitness[Double, _4]
  implicit val sow = mkBinaryWitness[Short, _6]
  implicit val inw = mkBinaryWitness[Int, _8]
  implicit val low = mkBinaryWitness[Long, _10]
  implicit val stw = mkBinaryWitness[String, _11]
  implicit val vbw = mkBinaryWitness[Vector[Byte], _11]
  implicit val tsw = mkBinaryWitness[ThriftStruct, _12]
  implicit val maw = mkBinaryWitness[Map[_, _], _13]
  implicit val sew = mkBinaryWitness[Set[_], _14]
  implicit val liw = mkBinaryWitness[List[_], _15]
}
