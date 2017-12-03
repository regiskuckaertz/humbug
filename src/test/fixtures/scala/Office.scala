package com.gu.contentapi.client.model.v1

sealed trait Office extends TEnum
case object UK extends Office
case object US extends Office
case object AUS extends Office

object Office extends TEnumCodec[Office]{
override def encode = {
case UK => 0

case US => 1

case AUS => 2
}

override def decode = {
case 0 => UK

case 1 => US

case 2 => AUS
}
}