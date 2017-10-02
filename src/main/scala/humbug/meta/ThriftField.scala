package humbug
package meta

import scodec.Codec
import scodec.stream.{ decode ⇒ D, encode ⇒ E, StreamDecoder, StreamEncoder }

import shapeless.Nat

/**
 * Type class witnessing that field with ID `N` is of type `Out`
 */
trait ThriftField[N <: Nat] {
  type Out

  def codec: Codec[Out]

  def decode: StreamDecoder[Out] = D.once(codec)

  def encode: StreamEncoder[Out] = E.once(codec)
}
