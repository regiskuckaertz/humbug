package contentatom.storyquestions

case class StoryQuestionsAtom(relatedStoryId: String,relatedStoryLinkType: RelatedStoryLinkType,title: String,editorialQuestions: Option[List[QuestionSet]]= None,userQuestions: Option[List[QuestionSet]]= None,notifications: Option[NotificationProviders]= None,closeDate: Option[DateTime]= None) extends TStruct

object StoryQuestionsAtom extends TStructCodec[StoryQuestionsAtom]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

val w6 = Witness(6)

val w7 = Witness(7)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,RelatedStoryLinkType]()

implicit val r3 = new TFieldCodec[w3.T,String]()

implicit val r4 = new TFieldCodec[w4.T,Option[List[QuestionSet]]]()

implicit val r5 = new TFieldCodec[w5.T,Option[List[QuestionSet]]]()

implicit val r6 = new TFieldCodec[w6.T,Option[NotificationProviders]]()

implicit val r7 = new TFieldCodec[w7.T,Option[DateTime]]()

override val defaults = HMap[TFieldCodec](w4.value -> None,w5.value -> None,w6.value -> None,w7.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.relatedStoryId,w2.value -> x.relatedStoryLinkType,w3.value -> x.title,w4.value -> x.editorialQuestions,w5.value -> x.userQuestions,w6.value -> x.notifications,w7.value -> x.closeDate)

override def decode = (m) => for {
relatedStoryId <- m.get(w1.value).orElse(defaults.get(w1.value))

relatedStoryLinkType <- m.get(w2.value).orElse(defaults.get(w2.value))

title <- m.get(w3.value).orElse(defaults.get(w3.value))

editorialQuestions <- m.get(w4.value).orElse(defaults.get(w4.value))

userQuestions <- m.get(w5.value).orElse(defaults.get(w5.value))

notifications <- m.get(w6.value).orElse(defaults.get(w6.value))

closeDate <- m.get(w7.value).orElse(defaults.get(w7.value))
} yield StoryQuestionsAtom(relatedStoryId,relatedStoryLinkType,title,editorialQuestions,userQuestions,notifications,closeDate)
}