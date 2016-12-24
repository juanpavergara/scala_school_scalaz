package scalaz_school

import org.scalatest.FunSuite
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


//    trait Trading[Account, Market, Order, ClientOrder, Execution, Trade] {
//      def clientOrders:                   ClientOrder => List[Order]
//      def execute(m: Market, a: Account): Order       => List[Execution]
//      def allocate(as: List[Account]):    Execution   => List[Trade]
//    }

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
          List("exec1")
      }

      def allocate(as: List[Account]): Kleisli[List, Execution, Trade] = kleisli[List, Execution, Trade]{
        exec => List("trade1")
      }

      def tradeGeneration(market: Market,broker: Account,clientAccounts: List[Account]) = {
        clientOrders andThen
          execute(market, broker) andThen
          allocate(clientAccounts)
      }
    }

    object traidingobj extends TradingInterpreter

    val res = traidingobj tradeGeneration("market","account", List("clientAccount1")) run "ClientOrder"

    assert(res == List("trade1"))
  }

}
