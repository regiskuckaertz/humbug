package com.gu.contentapi.client.model.v1

sealed trait TagType extends TEnum
case object CONTRIBUTOR extends TagType
case object KEYWORD extends TagType
case object SERIES extends TagType
case object NEWSPAPER_BOOK_SECTION extends TagType
case object NEWSPAPER_BOOK extends TagType
case object BLOG extends TagType
case object TONE extends TagType
case object TYPE extends TagType
case object PUBLICATION extends TagType
case object TRACKING extends TagType
case object PAID_CONTENT extends TagType

object TagType extends TEnumCodec[TagType] {
  override def encode = {
    case CONTRIBUTOR            ⇒ 0
    case KEYWORD                ⇒ 1
    case SERIES                 ⇒ 2
    case NEWSPAPER_BOOK_SECTION ⇒ 3
    case NEWSPAPER_BOOK         ⇒ 4
    case BLOG                   ⇒ 5
    case TONE                   ⇒ 6
    case TYPE                   ⇒ 7
    case PUBLICATION            ⇒ 8
    case TRACKING               ⇒ 9
    case PAID_CONTENT           ⇒ 10
  }
  override def decode = {
    case 0  ⇒ CONTRIBUTOR
    case 1  ⇒ KEYWORD
    case 2  ⇒ SERIES
    case 3  ⇒ NEWSPAPER_BOOK_SECTION
    case 4  ⇒ NEWSPAPER_BOOK
    case 5  ⇒ BLOG
    case 6  ⇒ TONE
    case 7  ⇒ TYPE
    case 8  ⇒ PUBLICATION
    case 9  ⇒ TRACKING
    case 10 ⇒ PAID_CONTENT
  }
}