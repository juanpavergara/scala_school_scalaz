package scalaz_school

import org.scalatest.FunSuite
import scalaz._
import Scalaz._

class FreeMonadSchool extends FunSuite{

  test("Smoke test"){
    assert(true)
  }

  test("Free monad first test"){

    type UserId = Int
    type UserName = String
    type UserPhoto = String

    final case class Tweet(userId: UserId, msg: String)
    final case class User(id: UserId, name: UserName, photo: UserPhoto)

    // Services represent web services we can call to fetch data
    sealed trait Service[A]
    final case class GetTweets(userId: UserId) extends Service[List[Tweet]]
    final case class GetUserName(userId: UserId) extends Service[String]
    final case class GetUserPhoto(userId: UserId) extends Service[String]

    // A request represents a request for data
    final case class Request[A](service: Service[A])

    def fetch[A](service: Service[A]): Free[Request, A] = Free.liftF(Request(service))

    object IdInterpreter extends (Request ~> Id.Id) {

      import Id._

      def apply[A](in: Request[A]): Id[A] = in match {
        case Request(service) =>
          service match {
              /*
              El compilador no pone problema por el retorno de este List.
              Hace la conversión implícita de List[Tweet] a Id[List[Tweet]]
               */
            case GetTweets(userId) =>
              println(s"Getting tweets for user $userId")
                List(Tweet(1, "Hi"), Tweet(2, "Hi"), Tweet(1, "Bye"))

            /*
            Para los siguientes case el compilador me dice que espera que la evaluación
            se de a Id[A] (que expande a A) y yo le estoy entregando una evaluacion a String :(
             */
            case GetUserName(userId) =>
              println(s"Getting user name for user $userId")
              userId match {
                case 1 => "Agnes"
                case 2 => "Brian"
                case _ => "Anonymous"
              }

            case GetUserPhoto(userId) =>
              println(s"Getting user photo for user $userId")
              userId match {
                case 1 => ":-)"
                case 2 => ":-D"
                case _ => ":-|"
              }
          }
      }
    }


    object Example {

      val theId: UserId = 1

      def getUser(id: UserId): Free[Request, User] =
        for {
          name  <- fetch(GetUserName(id))
          photo <- fetch(GetUserPhoto(id))
        } yield User(id, name, photo)

      val free: Free[Request, List[(String, User)]] =
        for {
          tweets <- fetch(GetTweets(theId))
          result <- (tweets map { tweet: Tweet =>
            for {
              user <- getUser(tweet.userId)
            } yield (tweet.msg -> user)
          }).sequenceU
        } yield result

      def run: List[(String, User)] =
        free.foldMap(IdInterpreter)
    }

    Example.run

    assert(true)
  }
}
