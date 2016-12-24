package scalaz_school.freemonad.interpreters

import scalaz._
import scalaz_school.freemonad.model.FreeMonadModel._
import scala.concurrent._

object FutureInterpreter extends (Request ~> Future) {

  def apply[A](in: Request[A]): Future[A] = in match {

    case Request(service) =>

      service match {
        case GetTweets(userId) =>
          println(s"Getting tweets for user $userId")

            import scala.concurrent.ExecutionContext.Implicits.global
            Future{
              List(Tweet(1, "Hi"), Tweet(2, "Hi"), Tweet(1, "Bye"))
            }

        case GetUserName(userId) =>
          println(s"Getting user name for user $userId")
          import scala.concurrent.ExecutionContext.Implicits.global
          userId match {
            case 1 => Future{"Agnes"}
            case 2 => Future{"Brian"}
            case _ => Future{"Anonymous"}
          }

        case GetUserId(userName) =>
          println(s"Getting user name for user $userName")
          import scala.concurrent.ExecutionContext.Implicits.global
          userName match {
            case "Agnes" => Future{1}
            case "Brian" => Future{2}
            case _ => Future{-1}
          }

        case GetUserPhoto(userId) =>
          println(s"Getting user photo for user $userId")
          import scala.concurrent.ExecutionContext.Implicits.global
          userId match {

            case 1 => Future{":-)"}
            case 2 => Future{":-D"}
            case _ => Future{":-|"}
          }

      }
  }
}