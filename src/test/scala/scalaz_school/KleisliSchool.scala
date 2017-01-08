package scalaz_school

import org.scalatest.FunSuite
import scala.concurrent.{Await, ExecutionContext, Future}
import scalaz._, Scalaz._
import Kleisli._
import scala.concurrent.duration._
import scala.language.postfixOps

/**
 * Created by juanpavergara on 10/17/16.
 */
class KleisliSchool extends FunSuite{

  test("Composicion de Kleislies"){

    type ClientOrder = String
    type Order = String
    type Market = String
    type Account = String
    type Execution = String
    type Trade = String

    trait Trading[Account, Market, Order, ClientOrder, Execution, Trade] {
      def clientOrders: Kleisli[List, ClientOrder, Order]
      def execute(m: Market, a: Account): Kleisli[List, Order, Execution]
      def allocate(as: List[Account]): Kleisli[List, Execution, Trade]
    }

    trait TradingInterpreter extends Trading [Account, Market, Order, ClientOrder, Execution, Trade]{

      def clientOrders: Kleisli[List, ClientOrder, Order] = kleisli[List, ClientOrder, Order]{
        co => List("order1")
      }

      def execute(m: Market, a: Account): Kleisli[List, Order, Execution] = kleisli[List, Order, Execution] {
        order =>
          List("exec1", "exec2")
      }

      def allocate(as: List[Account]): Kleisli[List, Execution, Trade] = kleisli[List, Execution, Trade]{
        exec => List(s"${exec} traded")
      }

      def tradeGeneration(market: Market,account: Account,clientAccounts: List[Account]) = {
        clientOrders             andThen
        execute(market, account) andThen
        allocate(clientAccounts)
      }
    }

    object traidingobj extends TradingInterpreter

    val res = traidingobj tradeGeneration("market","account", List("clientAccount1")) run "ClientOrder"

    println(res)

    assert(res == List("exec1 traded", "exec2 traded"))
  }



  test("Probando lo de Yuji"){

    type Event = String
    type UserWithLogin = String
    type Error = String

    trait Command[A] {
      type Response[B] = EitherT[Future, Error, B]
      def execute()(implicit ec: ExecutionContext): Kleisli[Response, A, List[Event]]
    }

    case class CreateUser() extends Command[String] {

      override def execute()(implicit ec: ExecutionContext) = kleisli[Response, UserWithLogin, List[Event]]{ ul: UserWithLogin =>
        //import scala.concurrent.ExecutionContext.Implicits.global
        val a : Future[Error \/ List[Event]] = Future(\/-(List("evento1")))
        EitherT(a)
      }
    }

    val cu = CreateUser()

    import scala.concurrent.ExecutionContext.Implicits.global
    // Cuando ejecuto tengo una Kleisli
    val r1 = cu.execute()
    // Un kleisli se tiene que *run* inyectando la dependencia
    // Y lo que se obtiene al ejecutar la Kleisli es un EitherT[Future] tal como lo dice la
    // definicion de execute. No hay forma de tener lo de dentro del future porque la
    // Kleisli evalua a EitherT[Future].
    val executed: cu.Response[List[Event]] = r1.run("VergaraUserLogin")

    // Al EitherT[Future] le puedo "sacar" el Future con .run
    val future: Future[Disjunction[Error, List[Event]]] = executed.run

    val res = Await.result(future, 5 seconds)

    assert(res == \/-(List("evento1")))

  }

}
