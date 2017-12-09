package com.gu.contentatom.thrift.atom.explainer

sealed trait DisplayType extends TEnum
case object FLAT extends DisplayType
case object EXPANDABLE extends DisplayType
case object CAROUSEL extends DisplayType

object DisplayType extends TEnumCodec[DisplayType] {
  override def encode = {
    case FLAT       ⇒ 0
    case EXPANDABLE ⇒ 1
    case CAROUSEL   ⇒ 2
  }
  override def decode = {
    case 0 ⇒ FLAT
    case 1 ⇒ EXPANDABLE
    case 2 ⇒ CAROUSEL
  }
}