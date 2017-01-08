package scalaz_school

import org.scalatest.FunSuite
import scalaz._, Scalaz._

/**
 * Created by juanpavergara on 1/7/17.
 */
class EitherTSchool extends FunSuite {

  test("Se debe poder componer Either con Option"){

    val x : Option[String \/ Int] = Some(\/-(1))
    val y : Option[String \/ Int] = Some(-\/("BOOM!"))
    val z : Option[String \/ Int] = None

    val etx = EitherT(x)
    val ety = EitherT(y)
    val etz = EitherT(z)

    // Composicion monadica de EitherT debe evaluar a EitherT
    // En esta composicion debes tener en cuenta que se hace flatmap sobre los dos efectos:
    // 1. Either (Disjunction porque estamos en scalaz).
    // 2. Option.
    val res1 = for{
      a <- etx
      b <- ety
    } yield a

    // Interesante que un EitherT se pueda "run" para obtener la mónada que se compuso con Either
    // que en esta caso es Option
    val resRun = res1.run

    // Dado que tanto x como y evaluaron a Some, la composicion de 2 Some es Some
    assert(resRun.isDefined)
    // Sin embargo la composicion incluyo un -\/ asi que ese valor de Disjuntion tuvo que
    // llevar toda la evaluación de Disjuntion a -\/
    assert(resRun == Some(-\/("BOOM!")))

    // Esta composicion prevalece el efecto de Option, pues etz es EitherT(None) y None lleva a None
    // siempre en flatmap
    val res2 = for{
      a <- etx
      b <- etz
    } yield a

    // Notar como res2.run se opera como un Option porque ES UN Option!
    assert(res2.run.getOrElse("EVALUACION A NONE") == "EVALUACION A NONE")
    assert(!res2.run.isDefined)
    assert(res2.run == None)

  }

//  test("Se debe poder componer Either (Disjunction) con Future") {
//    import scala.concurrent.ExecutionContext.Implicits.global
//
//    def foo(i:Int): DisjunctionT[Future[], Nothing, Nothing] = i%2 match {
//      case 0 => EitherT {
//          Future {
//            \/-(List("OK"))
//          }
//        }
//      case 1 =>
//          EitherT{
//            Future{
//              -\/("BOOOM!")
//            }
//          }
//    }
//
//    val res = for {
//      x <- foo(2)
//      y <- foo(1)
//    } yield y
//
//    println(s"res: ${res}")
//
//  }
}
