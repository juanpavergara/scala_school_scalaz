package scalaz_school.freemonad.interpreters

import scalaz._
import scalaz_school.freemonad.model.FreeMonadModel._


object OptionMockInterpreter extends (Request ~> Option) {

  def apply[A](in: Request[A]): Option[A] = in match {

    case Request(service) =>

      service match {
        case GetTweets(userId) =>
          println(s"Getting tweets for user $userId")
          None

        case GetUserName(userId) =>
          println(s"Getting user name for user $userId")

          userId match {
            case 1 => Some{"Agnes"}
            case 2 => Some{"Brian"}
            case _ => Some{"Anonymous"}
          }

        case GetUserId(userName) =>
          println(s"Getting user name for user $userName")

          userName match {
            case "Agnes" => Some{1}
            case "Brian" => Some{2}
            case _ => Some{-1}
          }

        case GetUserPhoto(userId) =>
          println(s"Getting user photo for user $userId")

          userId match {

            case 1 => Some{":-)"}
            case 2 => Some{":-D"}
            case _ => Some{":-|"}
          }

      }
  }
}