package scalaz_school.freemonad.program

import scalaz._
import Scalaz._
import scalaz_school.freemonad.interpreters.IdInterpreter
import scalaz_school.freemonad.model.FreeMonadModel._


object FreeMonadProgram {

  def fetch[A](service: Service[A]): Free[Request, A] =
    Free.liftF[Request, A](Request(service) : Request[A])

  val theId: UserId = 1

  /*
  Lo importante es comprender que fetch "monta" una mónada libre
  por cada evaluación. Dado que arroja monadas, son sujetas de ser encadenadas
  con flatmap o for-comp como cualquier monada.
  ¿Como se maneja concretamente el efecto? ¿De cual efecto se encarga una monada libre?
  La respuesta es: aquella a la que evalua la transformacion natural implementada en el interprete.

  Este programa solo es una declaracion. No se ejecuta en este momento. Solo se ejecuta cuando
  se entregue un interprete concreto.

   */
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

  /*
  Este programa en particualr se ejecutara cuando se llame este metodo.
  En este momento este metodo siempre eecuta con IdInterprete

  TODO: implementar un runWith(Request ~> Monad) para que el interprete pueda
  ser inyectado
   */
  def runWithId: List[(String, User)] =
    free.foldMap(IdInterpreter)

}

