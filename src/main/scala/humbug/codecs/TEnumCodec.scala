package humbug
package codecs

trait TEnumCodec[A] {
  def encode: A ⇒ Int
  def decode: Int ⇒ Option[A]
}
