package humbug
package codec

trait TTypeDefCodec[A <: TTypeDef, B] {
  def encode: A ⇒ B
  def decode: B ⇒ A
}
