package scalaz_school

import org.scalatest.FunSuite

import scala.concurrent.{Await, Future}


class Sandbox extends FunSuite{

  test("smoke"){
    assert(true)
  }

  test("Either one") {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    val o1 = Option(Future(1))
    val o2 = Option(Future(2))

    def sumarFuturos(f1:Future[Int], f2:Future[Int]): Future[Int] ={
      for{
        v1 <- f1
        v2 <- f2
      } yield v1 + v2
    }

    val res: Option[Future[Int]] = for {
      a <- o1
      b <- o2
    } yield sumarFuturos(a,b)

    val r: Option[Int] = res.map(of => {
      Await.result(of, Duration.Inf)
    })

    assert(r==Some(3))
    assert(r==Option(3))

  }


  test("For con futuro fallido"){

    import scala.concurrent.ExecutionContext.Implicits.global
    val f1: Future[Int] = Future(1)
    val f2: Future[Int] = Future.failed(new Exception("BOOM"))

    val res = for{
      a <- f1
      b <- f2
    }yield a + b

    val x = res.recoverWith{
      case e:Exception => Future(999)
    }

    import scala.concurrent.duration._
    val r = Await.result(x, Duration.Inf)

    assert(r == 999)

  }


  test("Future con Option dentro"){
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val v1: Future[Option[Int]] = Future(Option(1))
    val v2: Future[Option[Int]] = Future(None)
    def v3: Future[Option[Int]] = {
      println("Ejecutando v3")
      Future(Option(3))
    }

    def sumarOpciones(o1:Option[Int], o2:Option[Int]) ={
      for{
        a <- o1
        b <- o2
      }yield a + b
    }
    val res: Future[Option[Int]] = for {
      a <- v1
      b <- v2
      c <- v3
    }yield sumarOpciones(a,b)


  val r: Option[Int] = Await.result(res, Duration.Inf)

  println(r)

  }





}
