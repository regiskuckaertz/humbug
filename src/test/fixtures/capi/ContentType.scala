package com.gu.contentapi.client.model.v1

sealed trait ContentType extends TEnum
case object ARTICLE extends ContentType
case object LIVEBLOG extends ContentType
case object GALLERY extends ContentType
case object INTERACTIVE extends ContentType
case object PICTURE extends ContentType
case object VIDEO extends ContentType
case object CROSSWORD extends ContentType
case object AUDIO extends ContentType

object ContentType extends TEnumCodec[ContentType] {
  override def encode = {
    case ARTICLE     ⇒ 0

    case LIVEBLOG    ⇒ 1

    case GALLERY     ⇒ 2

    case INTERACTIVE ⇒ 3

    case PICTURE     ⇒ 4

    case VIDEO       ⇒ 5

    case CROSSWORD   ⇒ 6

    case AUDIO       ⇒ 7
  }

  override def decode = {
    case 0 ⇒ ARTICLE

    case 1 ⇒ LIVEBLOG

    case 2 ⇒ GALLERY

    case 3 ⇒ INTERACTIVE

    case 4 ⇒ PICTURE

    case 5 ⇒ VIDEO

    case 6 ⇒ CROSSWORD

    case 7 ⇒ AUDIO
  }
}