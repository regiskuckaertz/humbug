package humbug
package codec

import shapeless._

sealed abstract class StructWitness[A] {
  def value(a: A): Byte
}

object StructWitness {
  val (wTrue, wFalse) = (Witness(true), Witness(false))

  implicit case object TrueW   extends StructWitness[Boolean] {
    def value(b: Boolean): Byte = if(b) 1 else 2
  }
  implicit case object ByteW   extends StructWitness[Byte] {
    def value(b: Byte): Byte = 3
  }
  implicit case object ShortW  extends StructWitness[Short] {
    def value(b: Short): Byte = 4
  }
  implicit case object IntW    extends StructWitness[Int] {
    def value(b: Int): Byte = 5
  }
  implicit case object LongW   extends StructWitness[Long] {
    def value(b: Long): Byte = 6
  }
  implicit case object DoubleW extends StructWitness[Double] {
    def value(b: Double): Byte = 7
  }
  implicit case object BinaryW extends StructWitness[Array[Byte]] {
    def value(b: Array[Byte]): Byte = 8
  }
  implicit case object StringW extends StructWitness[String] {
    def value(b: String): Byte = 8
  }
  implicit case object ListW   extends StructWitness[List[_]] {
    def value(b: List[_]): Byte = 9
  }
  implicit case object SetW    extends StructWitness[Set[_]] {
    def value(b: Set[_]): Byte = 10
  }
  implicit case object MapW    extends StructWitness[Map[_, _]] {
    def value(b: Map[_, _]): Byte = 11
  }
  implicit case object StructW extends StructWitness[ThriftStruct] {
    def value(b: ThriftStruct): Byte = 12
  }

}
