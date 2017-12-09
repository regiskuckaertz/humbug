package com.gu.contentatom.thrift.atom.media

sealed trait Category extends TEnum
case object DOCUMENTARY extends Category
case object EXPLAINER extends Category
case object FEATURE extends Category
case object NEWS extends Category
case object HOSTED extends Category
case object PAID extends Category

object Category extends TEnumCodec[Category] {
  override def encode = {
    case DOCUMENTARY ⇒ 0
    case EXPLAINER   ⇒ 1
    case FEATURE     ⇒ 2
    case NEWS        ⇒ 3
    case HOSTED      ⇒ 4
    case PAID        ⇒ 5
  }
  override def decode = {
    case 0 ⇒ DOCUMENTARY
    case 1 ⇒ EXPLAINER
    case 2 ⇒ FEATURE
    case 3 ⇒ NEWS
    case 4 ⇒ HOSTED
    case 5 ⇒ PAID
  }
}