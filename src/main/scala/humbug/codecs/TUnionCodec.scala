package humbug
package codecs

trait TUnionCodec[A] {
  def encode: A ⇒ (FieldID, Dynamic)
  def decode: (FieldID, Dynamic) ⇒ Option[A]
}