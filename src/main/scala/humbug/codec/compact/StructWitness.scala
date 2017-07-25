package humbug
package codec
package compact

import shapeless._, shapeless.nat._, shapeless.ops.nat._

trait StructWitness[A] {
  type N <: Nat

  def value: Int
}

object StructWitness {
  val (wTrue, wFalse) = (Witness(true), Witness(false))

  private def mkStructWitness[A, N0 <: Nat](
    implicit
    toInt: ToInt[N0]
  ) = new StructWitness[A] {
    type N = N0

    val value = toInt()
  }

  implicit val trw = mkStructWitness[wTrue.T, _1]
  implicit val faw = mkStructWitness[wFalse.T, _2]
  implicit val byw = mkStructWitness[Byte, _3]
  implicit val shw = mkStructWitness[Short, _4]
  implicit val inw = mkStructWitness[Int, _5]
  implicit val low = mkStructWitness[Long, _6]
  implicit val dow = mkStructWitness[Double, _7]
  implicit val stw = mkStructWitness[String, _8]
  implicit val vbw = mkStructWitness[Vector[Byte], _8]
  implicit val liw = mkStructWitness[List[_], _9]
  implicit val sew = mkStructWitness[Set[_], _10]
  implicit val maw = mkStructWitness[Map[_, _], _11]
  implicit val tsw = mkStructWitness[ThriftStruct, _12]

}
