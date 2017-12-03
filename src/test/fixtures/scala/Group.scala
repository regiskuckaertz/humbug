package com.gu.storypackage.model.v1

sealed trait Group extends TEnum
case object Included extends Group
case object Linked extends Group

object Group extends TEnumCodec[Group] {
  override def encode = {
    case Included ⇒ 1
    case Linked   ⇒ 2
  }
  override def decode = {
    case 1 ⇒ Included
    case 2 ⇒ Linked
  }
}