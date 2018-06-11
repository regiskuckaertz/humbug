package humbug
package codecs

import scodec.{ Attempt, codecs ⇒ C, Codec, Err }

package object binary {
  val bool: Codec[Boolean] = C.ignore(7).dropLeft(C.bool)
  val byte: Codec[Byte] = C.byte
  val int8: Codec[Int] = C.int8
  val int16: Codec[Short] = C.short16
  val int32: Codec[Int] = C.int32
  val int64: Codec[Long] = C.int64
  val double: Codec[Double] = C.double
  val string: Codec[String] = C.variableSizeBytes(int32, C.utf8)

  trait TypeAndCodec {
    type A
    def codec: Codec[A]
    def `type`: Type[A]
  }

  def codecOf: Int ⇒ Option[TypeAndCodec] = {
    case 2  ⇒ Some(new TypeAndCodec { type A = Boolean; val codec = bool; val `type` = TyBool })
    case 3  ⇒ Some(new TypeAndCodec { type A = Byte; val codec = byte; val `type` = TyByte })
    case 4  ⇒ Some(new TypeAndCodec { type A = Double; val codec = double; val `type` = TyDouble })
    case 6  ⇒ Some(new TypeAndCodec { type A = Short; val codec = int16; val `type` = TyI16 })
    case 8  ⇒ Some(new TypeAndCodec { type A = Int; val codec = int32; val `type` = TyI32 })
    case 10 ⇒ Some(new TypeAndCodec { type A = Long; val codec = int64; val `type` = TyI64 })
    case 11 ⇒ Some(new TypeAndCodec { type A = String; val codec = string; val `type` = TyString })
    case 12 ⇒ Some(new TypeAndCodec { type A = Fields; val codec = struct; val `type` = TyStruct })
    case _  ⇒ None
  }

  // TODO: list of lists, list of sets, list of maps
  def list: Codec[Dynamic] = int8.flatZip { discriminator ⇒
    codecOf(discriminator) match {
      case Some(tac) ⇒ widenDynamic[List[tac.A]](C.listOfN(C.int32, tac.codec), TyList(tac.`type`))
      case None      ⇒ C.fail[Dynamic](Err(s"Type $discriminator isn't supported yet"))
    }
  }.xmap(_._2, { case d @ Dyn(_, typ) ⇒ (typ.typeId, d) })

  // TODO: list of lists, list of sets, list of maps
  def set: Codec[Dynamic] = int8.flatZip { discriminator ⇒
    codecOf(discriminator) match {
      case Some(tac) ⇒ widenDynamic[Set[tac.A]](C.listOfN(C.int32, tac.codec).xmap(_.toSet, _.toList), TySet(tac.`type`))
      case None      ⇒ C.fail[Dynamic](Err(s"Type $discriminator isn't supported yet"))
    }
  }.xmap(_._2, { case d @ Dyn(_, typ) ⇒ (typ.typeId, d) })

  // TODO: list of lists, list of sets, list of maps
  def map: Codec[Dynamic] = (int8 ~ int8).flatZip {
    case (discriminatork, discriminatorv) ⇒
      (codecOf(discriminatork), codecOf(discriminatorv)) match {
        case (Some(tack), Some(tacv)) ⇒
          widenDynamic[Map[tack.A, tacv.A]](C.listOfN(C.int32, tack.codec ~ tacv.codec).xmap(_.toMap, _.toList), TyMap(tack.`type`, tacv.`type`))
        case _ ⇒ C.fail[Dynamic](Err(s"Type $discriminatork/$discriminatorv isn't supported yet"))
      }
  }.xmap(_._2, { case d @ Dyn(_, typ) ⇒ ((typ.typeId), d) })

  val field: Codec[Dynamic] = C.discriminated[Dynamic].by(int8)
    .typecase(2, widenDynamic(bool, TyBool))
    .typecase(3, widenDynamic(byte, TyByte))
    .typecase(4, widenDynamic(double, TyDouble))
    .typecase(6, widenDynamic(int16, TyI16))
    .typecase(8, widenDynamic(int32, TyI32))
    .typecase(10, widenDynamic(int64, TyI64))
    .typecase(11, widenDynamic(string, TyString))
    .typecase(12, widenDynamic(struct, TyStruct))
    .typecase(13, map)
    .typecase(14, set)
    .typecase(15, list)

  private def widenDynamic[A](codec: Codec[A], typ: Type[A]): Codec[Dynamic] =
    codec.widen[Dynamic](Dyn(_, typ), d ⇒ Attempt.fromOption(Dynamic.cast(d, typ), Err(s"Dynamic not a $typ: $d")))

  val stop: Codec[Unit] = C.byte.unit(0)

  def struct: Codec[Fields] = ???

  // implicit def typedef[A <: TTypeDef, R](implicit A: TTypeDefCodec.Aux[A, R], C: Codec[R]): Codec[A] =
  //   C.xmap(A.decode, A.encode)

  // implicit def struct[A <: TStruct](implicit A: TStructCodec[A]): Codec[A] =
  //   C.list(C.short16 ~ dynamic).narrow(
  //     { a ⇒ Attempt.fromOption(A.decode(Map(a: _*)), Err(s"Cannot decode $a")) },
  //     { a: A ⇒ A.encode(a).toList }
  //   ).dropRight(stop)

  // implicit def union[A <: TUnion](implicit A: TUnionCodec[A]): Codec[A] =
  //   (C.short16 ~ dynamic).dropRight(stop).narrow(
  //     a ⇒ Attempt.fromOption(A.decode(a._1, a._2), Err(s"Cannot decode $a")),
  //     A.encode
  //   )
}