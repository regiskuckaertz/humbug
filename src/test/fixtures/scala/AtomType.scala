package com.gu.contentatom.thrift

sealed trait AtomType extends TEnum
case object QUIZ extends AtomType
case object MEDIA extends AtomType
case object EXPLAINER extends AtomType
case object CTA extends AtomType
case object INTERACTIVE extends AtomType
case object REVIEW extends AtomType
case object RECIPE extends AtomType
case object STORYQUESTIONS extends AtomType
case object QANDA extends AtomType
case object PROFILE extends AtomType
case object GUIDE extends AtomType
case object TIMELINE extends AtomType

object AtomType extends TEnumCodec[AtomType]{
override def encode = {
case QUIZ => 0

case MEDIA => 2

case EXPLAINER => 3

case CTA => 4

case INTERACTIVE => 5

case REVIEW => 6

case RECIPE => 7

case STORYQUESTIONS => 8

case QANDA => 9

case PROFILE => 10

case GUIDE => 11

case TIMELINE => 12
}

override def decode = {
case 0 => QUIZ

case 2 => MEDIA

case 3 => EXPLAINER

case 4 => CTA

case 5 => INTERACTIVE

case 6 => REVIEW

case 7 => RECIPE

case 8 => STORYQUESTIONS

case 9 => QANDA

case 10 => PROFILE

case 11 => GUIDE

case 12 => TIMELINE
}
}