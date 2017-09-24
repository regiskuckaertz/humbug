package humbug
package codec

trait ThriftEnumGeneric[A <: ThriftEnum] {
  def from: Int ⇒ A

  def to: A ⇒ Int
}
