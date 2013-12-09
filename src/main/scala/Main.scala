object Main {

  sealed trait Emotion

  case object Love extends Emotion

  case object Hate extends Emotion

  type User = String

  class Conversation(
                      affinity: Map[(User, User), Emotion],
                      message: Map[(User, Emotion), String]
                      ) {
    def generate_simple(u1: User, u2: User): Option[String] = {

      affinity.get(u1, u2).map {
        x =>
          val emotion = affinity(u1, u2)
          message.get(u1, emotion).map(_ => template(u1, u2, message(u1, emotion)))
      }.getOrElse(None)
    }

    def generate_complex(u1: User, u2: User): Option[String] = {
      val r = for {
        a1 <- affinity.get(u1, u2)
        a2 <- affinity.get(u2, u1)
      } yield (a1, a2)

      r match {
        case Some(x) if x._1 == x._2 => {
          message.get((u1, inverse(x._1))) match {
            case Some(r) => Some(template(u1, u2, message((u1, inverse(x._1)))))
            case None => {
              message.get((u1, x._1)).map(_ => template(u1, u2, message((u1, x._1))))
            }
          }
        }
        case None => None
      }
    }

    private def template(u1: User, u2: User, msg: String) = s"$u2「$msg？」\n$u1「$msg！」"

    private def inverse(e: Emotion) = e match {
      case Love => Hate
      case Hate => Love
    }
  }

}
