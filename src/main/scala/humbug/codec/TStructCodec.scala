package humbug
package codec

import shapeless._

trait TStructCodec[A <: TStruct] {
  def defaults: HMap[TFieldCodec]
  def encode: A ⇒ HMap[TFieldCodec]
  def decode: HMap[TFieldCodec] ⇒ Option[A]
}
