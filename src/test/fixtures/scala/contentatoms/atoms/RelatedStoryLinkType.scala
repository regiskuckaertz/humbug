package com.gu.contentatom.thrift.atom.storyquestions

sealed trait RelatedStoryLinkType extends TEnum
case object TAG extends RelatedStoryLinkType
case object STORY extends RelatedStoryLinkType

object RelatedStoryLinkType extends TEnumCodec[RelatedStoryLinkType] {
  override def encode = {
    case TAG   ⇒ 0
    case STORY ⇒ 1
  }
  override def decode = {
    case 0 ⇒ TAG
    case 1 ⇒ STORY
  }
}