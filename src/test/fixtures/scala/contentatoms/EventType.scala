package com.gu.contentatom.thrift

sealed trait EventType extends TEnum
case object UPDATE extends EventType
case object TAKEDOWN extends EventType

object EventType extends TEnumCodec[EventType] {
  override def encode = {
    case UPDATE   ⇒ 0
    case TAKEDOWN ⇒ 1
  }
  override def decode = {
    case 0 ⇒ UPDATE
    case 1 ⇒ TAKEDOWN
  }
}