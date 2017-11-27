package humbug
package codecs

import shapeless._

trait TStructCodec[A] {
  def defaults: HMap[TFieldCodec]
  def encode: A ⇒ HMap[TFieldCodec]
  def decode: HMap[TFieldCodec] ⇒ Option[A]
}
