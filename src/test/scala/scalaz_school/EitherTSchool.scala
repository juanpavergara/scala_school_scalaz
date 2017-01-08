package scalaz_school

import org.scalatest.FunSuite
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
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

  test("Se debe poder componer Either (Disjunction) con Future") {

    import scala.concurrent.ExecutionContext.Implicits.global

    val x : Future[String \/ Int] = Future(\/-(6))
    val y : Future[String \/ Int] = Future(-\/("BOOM!"))
    val z : Future[String \/ Int] = Future.failed(new Exception("BOOM!"))

    val etx = EitherT(x)
    val ety = EitherT(y)
    val etz = EitherT(z)

    val r1 = for {
      x <- etx
      y <- ety
    } yield x

    // Saca el futuro del EitherT con r.run (ok) y saca el valor del Future con Await (no ok)
    val res1 = Await.result(r1.run, 5 seconds)

    // Note como el resultado es de la izquierda en la Disjunction, pues este valor rompe la estructura
    // en ese efecto monádico. Solo por estar presente ety que viene de y que es un -\/ ya todo el computo evalua
    // a izquierda
    assert(res1 == -\/("BOOM!"))

    // En este caso se verifica que todo
    // el programa se ejecuta cuando las dos funciones evaluan a futuro exitoso de derecha
    val r2 = for {
      x <- etx
      y <- etx
    } yield x

    val res2 = Await.result(r2.run, 5 seconds)

    assert(res2 == \/-(6))

  }
}
