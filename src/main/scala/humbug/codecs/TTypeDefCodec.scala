package humbug
package codecs

trait TTypeDefCodec[A, B] {
  def encode: A ⇒ B
  def decode: B ⇒ A
}
