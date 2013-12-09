object Main {
  sealed trait Emotion
  case object Love extends Emotion
  case object Hate extends Emotion

  type User = String

  class Conversation(
    affinity: Map[(User, User), Emotion],
    message: Map[(User, Emotion), String]
  ){
    def generate_simple(u1: User, u2: User): Option[String] = {
      if(affinity.contains((u1, u2))){
        val emotion = affinity((u1, u2))
        if(message.contains((u1, emotion))){
          Some(template(u1, u2, message((u1, emotion))))
        } else None
      } else None
    }

    def generate_complex(u1: User, u2: User): Option[String] = {
      if(affinity.contains((u1, u2)) && affinity.contains((u2, u1))){
        val emotion1 = affinity((u1, u2))
        val emotion2 = affinity((u2, u1))
        if(emotion1 == emotion2) {
          if(message.contains((u1, inverse(emotion1)))){
            Some(template(u1, u2, message((u1, inverse(emotion1)))))
          }else if(message.contains((u1, emotion1))){
            Some(template(u1, u2, message((u1, emotion1))))
          }else None
        } else None
      } else None
    }

    private def template(u1: User, u2: User, msg: String) = s"$u2「$msg？」\n$u1「$msg！」"

    private def inverse(e: Emotion) = e match {
      case Love => Hate
      case Hate => Love
    }
  }
}
