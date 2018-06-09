package humbug
package codecs

trait TTypeDefCodec[A] {
  type Rep
  def typeRep: Type[Rep]
  def encode: A ⇒ Rep
  def decode: Rep ⇒ A
}

object TTypeDefCodec {
  type Aux[A, R] = TTypeDefCodec[A] { type Rep = R }
}