package humbug
package codec

trait TEnumCodec[A <: TEnum] {
  def encode: Function1[A, Int]
  def decode: PartialFunction[Int, A]
}
