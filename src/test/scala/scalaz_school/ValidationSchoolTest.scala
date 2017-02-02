package scalaz_school

import org.scalatest.FunSuite

import scalaz._
import Scalaz._
import scalaz.std._
import _root_.java.io.Serializable

class ValidationSchoolTest extends FunSuite{

  test("Smoke test"){
    assert(true)
  }

  test("Construccion basica de un Validation - Success"){

    def foo: Validation[Nothing, Int] = {
      2.success
    }

    val res = foo

    assert(res == Success(2))

  }

  test("Construccion basica de un Validation - Failure"){

    def foo: Validation[Int, Nothing] = {
      2.failure
    }

    val res = foo

    assert(res == Failure(2))

  }

  test("Construccion de un Validation completo"){

    def foo(i:Int): Validation[Throwable, String] = i%2 match {
      case 0 => "PAR".success
      case _ => Failure(new Exception("IMPAR"))
    }

    println(foo(1))

    assert(foo(2) == Success("PAR"))
    assert(foo(1).isFailure)

  }

  test("Acumulacion de errores - EXITO"){

    def foo(i:Int): ValidationNel[Throwable, String] = i%2 match {
      case 0 => "PAR".successNel
      case _ => Failure(NonEmptyList(new Exception("IMPAR")))//FailureNel(new Exception("IMPAR"))
    }

    val res = (foo(2)|@|
      foo(0) |@|
      foo(6)){_+_+_}

    assert(res == Success("PARPARPAR"))

  }

  test("Acumulacion de errores - ERROR (1)"){

    def foo(i:Int): ValidationNel[Throwable, String] = i%2 match {
      case 0 => "PAR".successNel
      case _ => Failure(NonEmptyList(new Exception("IMPAR")))//FailureNel(new Exception("IMPAR"))
    }

    val res = (foo(2)|@|
      foo(1) |@|
      foo(6)){_+_+_}

    println(res)

    assert(res.isFailure)

  }

  test("Acumulacion de errores - ERROR (2)"){

    def foo(i:Int): ValidationNel[Throwable, String] = i%2 match {
      case 0 => "PAR".successNel
      case _ => Failure(NonEmptyList(new Exception("IMPAR")))//FailureNel(new Exception("IMPAR"))
    }

    val res = (foo(1)|@|
      foo(3) |@|
      foo(3)){_+_+_}

    res.fold(
      { errorResponse =>
        println(s"Xml validation failed: $errorResponse")
        errorResponse.map{
          error => println(s"error: ${error}")
        }
      }, { entity =>
        println(s"Entity : ${entity}")
      }
    )



  }

}
