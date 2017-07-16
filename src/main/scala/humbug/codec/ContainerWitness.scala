package humbug
package codec

import shapeless._, shapeless.nat._, shapeless.ops.nat._

trait ContainerWitness[A] {
  type N <: Nat

  def value: Int
}

object ContainerWitness {
  def mkContainerWitness[A, N0 <: Nat](implicit toInt: ToInt[N0]) =
    new ContainerWitness[A] {
      type N = N0

      val value: Int = toInt()
    }

  implicit val bow = mkContainerWitness[Boolean, _2]
  implicit val byw = mkContainerWitness[Byte, _3]
  implicit val dow = mkContainerWitness[Double, _4]
  implicit val sow = mkContainerWitness[Short, _6]
  implicit val inw = mkContainerWitness[Int, _8]
  implicit val low = mkContainerWitness[Long, _10]
  implicit val stw = mkContainerWitness[String, _11]
  implicit val vbw = mkContainerWitness[Vector[Byte], _11]
  implicit val tsw = mkContainerWitness[ThriftStruct, _12]
  implicit val maw = mkContainerWitness[Map[_, _], _13]
  implicit val sew = mkContainerWitness[Set[_], _14]
  implicit val liw = mkContainerWitness[List[_], _15]
}
