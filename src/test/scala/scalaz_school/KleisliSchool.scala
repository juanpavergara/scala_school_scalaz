package scalaz_school

import org.scalatest.FunSuite
import scala.concurrent.{ExecutionContext, Future}
import scalaz._, Scalaz._
import Kleisli._

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



//  test("Probando lo de Yuji"){
//
//    type Event = String
//    type UserWithLogin = String
//    type ErrorYuji = String
//
//    trait Command[A] {
//      type Response[B] = EitherT[Future, ErrorYuji, B]
//      def execute: Kleisli[Response, A, List[Event]]
//    }
//
//    case class CreateUser()(implicit ec: ExecutionContext) extends Command[UserWithLogin] {
//
//      override def execute: Kleisli[Response, UserWithLogin, List[Event]] = Kleisli { ul =>
//        import scala.concurrent.ExecutionContext.Implicits.global
//        EitherT.right{
//          Future{
//            List("OK")
//          }
//        }
//      }
//    }
//  }

}
