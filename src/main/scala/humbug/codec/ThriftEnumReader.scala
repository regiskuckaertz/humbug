package humbug
package codec

trait ThriftEnumReader[A <: ThriftEnum] {
  def from(x: Int): Option[A]
}
