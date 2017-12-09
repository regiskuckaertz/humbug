package com.gu.contentatom.thrift.atom.storyquestions

sealed trait AnswerType extends TEnum
case object CONTENT extends AnswerType
case object ATOM extends AnswerType

object AnswerType extends TEnumCodec[AnswerType] {
  override def encode = {
    case CONTENT ⇒ 0
    case ATOM    ⇒ 1
  }
  override def decode = {
    case 0 ⇒ CONTENT
    case 1 ⇒ ATOM
  }
}