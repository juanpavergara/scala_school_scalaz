package scalaz_school.freemonad.interpreters

import scalaz.{Id, _}
import scalaz_school.freemonad.model.FreeMonadModel._


/*
Este IdInterpreter es tomado del siguiente blog
http://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html
con las mejoras y adecuaciones hechas para compilar con scalaz 7.2 las cuales encontré en
https://gist.github.com/klaeufer/fcc1e1d96c2ff5717d85

Un ejemplo muy similar pero implementado con cats (en lugar de scalaz) y con
comentarios detallados lo puedes encontrar en
https://github.com/pvillega/free-monad-sample/blob/master/src/main/scala/com/perevillega/freemonad/OrdersSample.scala
Con su respectiva explicacion en el siguiente blog post
http://perevillega.com/understanding-free-monads
 */

object IdInterpreter extends (Request ~> Id.Id) {

  /*
  Esta implementacion se denomina 'interprete' y sera la ejecucion concreta del
  programa.

  Este interprete tiene dos caracteristicas:
    1. Se vale de la monada Id (monada identidad) para poder encadenar computos.
    Esto quiere decir que no se hace nada diferente a encadenar sin **ningun** efecto
    colateral (side effect).
    2. Es una implementacion de muestra, asi que se devuelven datos 'quemados' en el codigo
    para poder ver el programa corriendo en el test.

  A un interprete se le denomina 'transformacion natural'. Aunque eso no es necesario
  para entender el funcionamiento práctico, es la base teorica detras de la separacion programa / implementacion
  en monadas libres. Esto lo puedes ver en la definicion de este object con el
  extends (Request ~> Id.Id)

  El operador ~> de scalaz se llama Natural Transformation.
   */

  import Id._

  def apply[A](in: Request[A]): Id[A] = in match {

      /*
      En cada caso se debe manejar qué hacer. La unica condicion por cumplir
      es evaluar la expresion final a la monada de salida. En este caso Id.
      Cada interprete siempre debe evaluar a la misma monada, sin importar qué haga para llegar a ella.
      Aqui es donde genera valor una monada libre, dado que es en el interprete donde
      el desarrollador maneja el (o los) efectos colaterales.
       */
    case Request(service) =>

      service match {

        case GetTweets(userId) =>
          println(s"Getting tweets for user $userId")
          //TODO: Entender cómo se genera un Id[List[Tweet]] a partir de la List[Tweet]
          List(Tweet(1, "Hi"), Tweet(2, "Hi"), Tweet(1, "Bye"))

        case GetUserName(userId) =>
          println(s"Getting user name for user $userId")
          userId match {
            case 1 => "Agnes"
            case 2 => "Brian"
            case _ => "Anonymous"
          }

        case GetUserId(userName) =>
          println(s"Getting user name for user $userName")
          userName match {
            case "Agnes" => 1
            case "Brian" => 2
            case _ => -1
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