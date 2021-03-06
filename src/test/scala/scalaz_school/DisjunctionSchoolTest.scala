package scalaz_school

import org.scalatest.FlatSpec
import scalaz._
import Scalaz._
import scalaz.syntax.either._

class DisjunctionSchoolTest extends FlatSpec{

  behavior of "A Disjunction"

  it should " work with two right side Disjunctions" in {

    val success1 = \/-("This succeed")
    val success2 = \/-("This succeed also")

    val res1 = for{
      one <- success1
      two <- success2
    } yield (one, two)

    assert(res1.getOrElse("") == ("This succeed","This succeed also"))

  }

  it should " stop a for-com when a left is present" in {
    val success1 = \/-("I am happy")
    val failure1 = -\/("Boooom!!!")


    val res1 = for{
      one <- failure1
      two <- success1
    } yield (one, two)

    assert(res1.isLeft)

  }

  it should " stop for real a for-com when a left is present" in {

    lazy val success = \/-{
      println("This should be printed out")
      "Successful computation"
    }
    val failure = -\/("Boooom!!!")


    val res1 = for{
      one <- failure
      two <- success
    } yield (one, two)

    assert(res1.isLeft)

  }


  it should "be different to Validation from scalaz" in {


    case class A(a:String, b:String)

    val a = "event 1 ok".success[String]
    val b = "event 1 failed!".failure[String]

    val c = (a |@| b) {A(_,_)}

    println(c)

  }

  it should "sequenceU de list disjunction todos right" in {
    def foo(): \/[Exception, String] = \/-("Hola")
    val l= List(foo, foo)
    val r = l.sequenceU
    println(s"Hola niño 0 ${r}")
  }

  it should "sequenceU de list disjunction un right y un left" in {
    def foo(): \/[Exception, String] = \/-("Hola")
    def bar(): \/[Exception, String] = -\/(new Exception("Boom"))
    val l= List(foo, bar)
    val r = l.sequenceU

    println(s"Hola niño 1 ${r}")
  }

}