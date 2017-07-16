package humbug
package internal

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

import shapeless._, shapeless.ops.hlist

trait PositionedGeneric[T] {
  type Repr

  def to(t: T): Repr

  def from(r: Repr): T
}

object PositionedGeneric {
  type Aux[T, Repr0] = PositionedGeneric[T] { type Repr = Repr0 }

  def apply[T](implicit pgen: PositionedGeneric[T]): Aux[T, pgen.Repr] = pgen

  /** Handles the Product case (fields in a case class, for example) */
  implicit def materializeProduct[T, I <: HList, V <: HList, R <: HList]
    (implicit
      idx: Indices.Aux[T, I],
      gen: Generic.Aux[T, V],
      zip: hlist.ZipWithKeys.Aux[I, V, R],
      ev: R <:< V
    ): Aux[T, R] =
    new PositionedGeneric[T] {
      type Repr = R
      def to(t: T): Repr = gen.to(t).zipWithKeys(idx.keys)
      def from(r: Repr): T = gen.from(r)
    }
}

trait Indices[T] {
  type Repr <: HList

  def keys: Repr
}

object Indices {
  type Aux[T, Repr0 <: HList] = Indices[T] { type Repr = Repr0 }
}
