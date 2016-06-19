package scalaz_school

import org.scalatest.FunSuite
import scalaz._
import scalaz_school.freemonad.program.FreeMonadProgram
import scalaz_school.freemonad.model.FreeMonadModel._

class FreeMonadSchoolTest extends FunSuite{

  test("Smoke test"){
    assert(true)
  }

  test("Free monad with IdInterpreter"){
    val result: List[(String, User)] = FreeMonadProgram.runWithId
    assert(result.size == 3)
    assert(result.filter(x => x._1 == "Bye").head._2.photo.equals(":-)"))
  }

}
