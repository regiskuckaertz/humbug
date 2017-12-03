package com.gu.storypackage.model.v1

sealed trait ArticleType extends TEnum
case object Article extends ArticleType
case object Snap extends ArticleType

object ArticleType extends TEnumCodec[ArticleType] {
  override def encode = {
    case Article ⇒ 1
    case Snap    ⇒ 2
  }
  override def decode = {
    case 1 ⇒ Article
    case 2 ⇒ Snap
  }
}