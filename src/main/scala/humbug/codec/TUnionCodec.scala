package humbug
package codec

import shapeless._

trait TUnionCodec[A <: TUnion] {
  def encode: A ⇒ HMap[TFieldCodec]
  def decode: HMap[TFieldCodec] ⇒ Option[A]
}
