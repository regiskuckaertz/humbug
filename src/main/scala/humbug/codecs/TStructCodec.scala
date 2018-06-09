package humbug
package codecs

trait TStructCodec[A] {
  def fieldIds: List[FieldID]
  def defaults: Fields
  def encode: A ⇒ Fields
  def decode: Fields ⇒ Option[A]
}