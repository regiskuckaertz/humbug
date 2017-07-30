package humbug
package codec

trait ThriftEnumGeneric[A <: ThriftEnum] {
  def from: Int => Option[A]

  def to: A => Int
}
