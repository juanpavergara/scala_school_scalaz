package scalaz_school.freemonad.model


object FreeMonadModel {

  type UserId = Int
  type UserName = String
  type UserPhoto = String

  type Tweets = List[String]

  final case class Tweet(userId: UserId, msg: String)
  final case class User(id: UserId, name: UserName, photo: UserPhoto)

  // Servicios que representan consumos de servicios remotos
  sealed trait Service[A]
  final case class GetTweets(userId: UserId) extends Service[List[Tweet]]
  final case class GetUserName(userId: UserId) extends Service[String]
  final case class GetUserPhoto(userId: UserId) extends Service[String]
  final case class GetUserId(userId: UserName) extends Service[Int]


  // Representacion de una solicitud de datos
  final case class Request[A](service: Service[A])

  case class Response[A](res: A)
}
