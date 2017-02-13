package scalaz_school

import org.scalatest.FunSuite
import scalaz._
import Scalaz._

class OptionTSchoolTest extends FunSuite{

  test("smoke test"){
    assert(true)
  }

  test("Primer test con OptionT (Future + None)"){
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.language.postfixOps

    def foo = OptionT[Future, Int]{
      Future(Some(1))
    }

    def bar = OptionT[Future, Int]{
      Future(None)
    }

    val resOptionFuture: OptionT[Future, Int] = for{
      x <- bar
      y <- foo
    } yield x + y


    val r1: Future[Option[Int]] = resOptionFuture.run
    val r2: Option[Int] = Await.result(r1, Duration.Inf)

    assert(r2 == None)


  }

  test("Primer test con OptionT (Future + Some)"){
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.language.postfixOps

    def foo = OptionT[Future, Int]{
      Future(Some(1))
    }

    def bar = OptionT[Future, Int]{
      Future(Some(1))
    }

    val resOptionFuture: OptionT[Future, Int] = for{
      x <- bar
      y <- foo
    } yield x + y


    val r1: Future[Option[Int]] = resOptionFuture.run
    val r2: Option[Int] = Await.result(r1, Duration.Inf)

    assert(r2 == Some(2))

  }

  test("Primer test con OptionT (Future fallido)"){
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.language.postfixOps

    def foo = OptionT[Future, Int]{
      Future(Some(1))
    }

    def bar = OptionT[Future, Int]{
      Future.failed(new Exception("BOOM"))
    }

    val resOptionFuture: OptionT[Future, Int] = for{
      x <- bar
      y <- foo
    } yield x + y


    val r1: Future[Option[Int]] = resOptionFuture.run.recover{case e:Exception => Some(666)}
    val r2 = Await.result(r1, Duration.Inf)

    assert(r2 == Some(666))

  }

}
