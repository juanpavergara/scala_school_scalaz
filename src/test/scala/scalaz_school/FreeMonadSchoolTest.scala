package scalaz_school

import org.scalatest.FunSuite
import scala.concurrent.Await
import scala.concurrent.duration._

import scalaz_school.freemonad.program.FreeMonadProgram
import scalaz_school.freemonad.model.FreeMonadModel._

class FreeMonadSchoolTest extends FunSuite{

  test("Smoke test"){
    assert(true)
  }

  test("Free monad with IdInterpreter"){
    val idresult: List[(String, User)] = FreeMonadProgram.runWithId
    assert(idresult.size == 3)
    assert(idresult.filter(x => x._1 == "Bye").head._2.photo.equals(":-)"))
  }

  test("Free monad with FutureInterpreter"){
    val fresult = FreeMonadProgram.runWithFuture
    val r = Await.result(fresult, 10 seconds)
    assert(r.size == 3)
    assert(r.filter(x => x._1 == "Bye").head._2.photo.equals(":-)"))
  }

  test("Free monad with OptionInterpreter"){
    val oresult = FreeMonadProgram.runWithOption
    // Lo interesante de este test es que el interprete de Option en la operación
    // GetTweets(userId) evalúa a None. Esta implementacion de la operacion es
    // diferente a la de IdInterpreter y FutureInterprete.
    // Esto hace que el programa evalúe completo a None.
    // Este caso demuestra el poder de Free Monad porque demuestra que la interpretacion (implementacion)
    // del programa puede variar completamente, siempre y cuando cumpla con las operaciones y la monada
    // a la que evalua. El efecto monadico dictará entonces cómo se encadenan los computos en el flatmap.
    assert(!oresult.isDefined)
    assert(oresult.getOrElse(List()) == List())

  }

}
