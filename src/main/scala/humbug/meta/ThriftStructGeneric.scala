package humbug
package meta

/**
 * Let's say I have the following struct:
 * ```
 * struct Person {
 *   1: required int32 id
 * }
 * ```
 * it will be compiled into
 * ```
 * case class Person(id: Int)
 * ```
 * we need an instance of ThriftStructGeneric[Person]
 * that looks like this:
 * ```
 * ThriftStructGeneric[Person] {
 *   object ThriftFieldId extends ThriftField[Succ[Zero]] { type Out = Int }
 * }
 * ```
 * this object must be available implicitly so that it can
 * be summoned when encoding/decoding a struct
 *
 * We also need a way to reify a Succ[Zero] at runtime out of an arbitrary
 * int16 coming from the network. Because shapeless macros only work with
 * literals, we don't have much choice but to introduce a map of Ints to Nats:
 * ```
 * val fieldIds = Map(
 *   1 -> _1
 * )
 * ```
 * Hence the final form of our generic:
 * ```
 * ThriftStructGeneric[Person] {
 *   implicit val fieldIds: Map[Int, Nat] = Map(
 *     1 -> _1
 *   )
 *   implicit val thriftFieldId = new ThriftField[Succ[Zero]] { type Out = Int }
 * }
 * ```
 * The compiler will put all these objects into the companion object:
 * ```
 * object Person {
 *   implicit val personGeneric = new ThriftStructGeneric[Person] {
 *     implicit val fieldIds: Map[Int, Nat] = Map(
 *       1 -> _1
 *     )
 *     implicit val thriftFieldId = new ThriftField[Succ[Zero]] { type Out = Int }
 *   }
 * }
 * ```
 */

import shapeless._
import shapeless.ops.hlist.Length

trait ThriftStructGeneric[A <: ThriftStruct] {
  type Repr <: HList

  implicit def fieldMap: Map[Short, (Nat, ThriftField[Nat])]

  implicit def default: Repr

  implicit def length(implicit L: Length[Repr]): Nat = L()
}
