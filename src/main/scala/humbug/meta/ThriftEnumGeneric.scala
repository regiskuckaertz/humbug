package humbug
package meta

trait ThriftEnumGeneric[A <: ThriftEnum] {
  def from: Int ⇒ A

  def to: A ⇒ Int
}
