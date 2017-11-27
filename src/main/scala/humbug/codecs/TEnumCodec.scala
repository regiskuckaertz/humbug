package humbug
package codecs

trait TEnumCodec[A <: TEnum] {
  def encode: Function1[A, Int]
  def decode: PartialFunction[Int, A]
}
