package scalaz_school

import org.scalatest.FunSuite

import scalaz._
import Scalaz._
import scala.util._

class ReaderTSchoolTest extends FunSuite{
  test("Smoke test"){
    assert(true)
  }

  test("Stacking Reader with Option (None)"){
    def foo() = ReaderT[Option,Int, Int]{
      case i:Int => Some(i)
    }

    def bar() = ReaderT[Option,Int, Int]{
      case i:Int => None
    }

    val program: ReaderT[Option, Int, Int] = for{
      a <- foo
      b <- bar
    } yield a + b

    val res = program.run(1)
    assert(res==None)
  }

  test("Stacking Reader with Option (Some)"){
    def foo() = ReaderT[Option,Int, Int]{
      case i:Int => Some(i)
    }

    def bar() = ReaderT[Option,Int, Int]{
      case i:Int => Some(i+2)
    }

    val program: ReaderT[Option, Int, Int] = for{
      a <- foo
      b <- bar
    } yield a + b

    val res = program.run(1)
    assert(res==Some(4))
  }

  test("Stacking Reader with Future (Failed)"){

    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.language.postfixOps

    def foo() = ReaderT[Future,Int, Int]{
      case i:Int => Future(i+1)
    }

    def bar() = ReaderT[Future,Int, Int]{
      case i:Int => Future.failed(new Exception("BOOM"))
    }

    val program: ReaderT[Future, Int, Int] = for{
      a <- bar
      b <- foo
    } yield a + b

    val resFuture: Future[Int] = program.run(1).recover{case e:Exception => 666}
    val res = Await.result(resFuture, Duration.Inf)
    println(res)
    assert(res == 666)
  }

  test("Stacking Reader with Future (Succesful)"){

    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.language.postfixOps

    def foo() = ReaderT[Future,Int, Int]{
      case i:Int => Future(i+1)
    }

    def bar() = ReaderT[Future,Int, Int]{
      case i:Int => Future(i+2)
    }

    val program: ReaderT[Future, Int, Int] = for{
      a <- bar
      b <- foo
    } yield a + b

    val resFuture: Future[Int] = program.run(1).recover{case e:Exception => 666}
    val res = Await.result(resFuture, Duration.Inf)

    assert(res == 5)
  }

  test("Stacking Reader with Disjunction (Right)"){

    type myType[A] = Disjunction[Throwable,A]

    def foo() = ReaderT[myType,Int, Int]{
      case i:Int => i%2 match {
        case 0 => \/-(i)
        case 1 => -\/(new Exception("ES IMPAR :("))
      }
    }


    val resReader = for{
      x <- foo
      y <- foo
    } yield x + y

    val resDisjunction = resReader.run(2)

    assert(resDisjunction == \/-(4))

  }


  test("Stacking Reader with Disjunction (Left)"){

    type myType[A] = Disjunction[Throwable,A]

    def foo() = ReaderT[myType,Int, Int]{
      case i:Int => i%2 match {
        case 0 => \/-(i)
        case 1 => -\/(new Exception("ES IMPAR :("))
      }
    }


    val resReader = for{
      x <- foo
      y <- foo
    } yield x + y

    val resDisjunction = resReader.run(1)

    assert(resDisjunction.isLeft)

  }

}
