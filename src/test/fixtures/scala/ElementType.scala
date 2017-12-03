package com.gu.contentapi.client.model.v1

sealed trait ElementType extends TEnum
case object TEXT extends ElementType
case object IMAGE extends ElementType
case object EMBED extends ElementType
case object FORM extends ElementType
case object PULLQUOTE extends ElementType
case object INTERACTIVE extends ElementType
case object COMMENT extends ElementType
case object RICH_LINK extends ElementType
case object TABLE extends ElementType
case object VIDEO extends ElementType
case object TWEET extends ElementType
case object WITNESS extends ElementType
case object CODE extends ElementType
case object AUDIO extends ElementType
case object MAP extends ElementType
case object DOCUMENT extends ElementType
case object MEMBERSHIP extends ElementType
case object INSTAGRAM extends ElementType
case object CONTENTATOM extends ElementType
case object VINE extends ElementType

object ElementType extends TEnumCodec[ElementType]{
override def encode = {
case TEXT => 0

case IMAGE => 1

case EMBED => 2

case FORM => 3

case PULLQUOTE => 4

case INTERACTIVE => 5

case COMMENT => 6

case RICH_LINK => 7

case TABLE => 8

case VIDEO => 9

case TWEET => 10

case WITNESS => 11

case CODE => 12

case AUDIO => 13

case MAP => 14

case DOCUMENT => 15

case MEMBERSHIP => 16

case INSTAGRAM => 17

case CONTENTATOM => 18

case VINE => 19
}

override def decode = {
case 0 => TEXT

case 1 => IMAGE

case 2 => EMBED

case 3 => FORM

case 4 => PULLQUOTE

case 5 => INTERACTIVE

case 6 => COMMENT

case 7 => RICH_LINK

case 8 => TABLE

case 9 => VIDEO

case 10 => TWEET

case 11 => WITNESS

case 12 => CODE

case 13 => AUDIO

case 14 => MAP

case 15 => DOCUMENT

case 16 => MEMBERSHIP

case 17 => INSTAGRAM

case 18 => CONTENTATOM

case 19 => VINE
}
}