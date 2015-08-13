package scalaz_school

import org.scalatest.FlatSpec
import scalaz._, Scalaz._

class FunctorSchoolTest extends FlatSpec{

  behavior of "A Functor"

  it should " map over the result of another function" in {

    // This test shows how scalaz can treat a function as a Functor :)

    val f = (x:Int) => x + 1
    val g = (x:Int) => x * 2

    val a = f map g

    val res = a(1)

    // Please remember that the evaluation of a is not as in f compose g but f map g.
    // f gets evaluated and the result is then passed to g which is different to f compose g
    assert(res == 4)
  }

  it should " map over a tuple" in {

    // This is a really cool case which you would wanna understand
    // why it apply the mapping function only to the last element :)
    val tuple = (1,2) map (_+1)
    assert(tuple == (1, 3))
  }

  it should " let us lift our own types (List) into a Functor" in {
    val f: (List[Int]) => List[Int] = Functor[List].lift{x:Int => x*2}
    val res = f(List(1, 2, 3))
    assert(res == List(2, 4, 6))
  }

  it should " let us map our own types (Own) with a custom Functor" in {

    case class MyCaseClass[A, B](a:A, b:B)
    case class MyContainer[A](a:A)

    val localCaseClass = MyCaseClass[String, Int]("Uno", 1)
    val localContainer = MyContainer[MyCaseClass[String, Int]]( a = localCaseClass )

    implicit val myCaseClassFunctor = new Functor[MyContainer]{
      def map[A,B](fa: MyContainer[A])(f:A=>B) = MyContainer(f(fa.a))
    }

    val F = Functor[MyContainer]

    def f(x:MyCaseClass[String, Int]): Int = {
      x.b * 2
    }

    def g(x:MyCaseClass[String, Int]): String = {
      "HOLA"
    }

    val res1 = F.map(localContainer)(f)
    val res2 = F.map(localContainer)(g)

    assert(res1.a == 2)
    assert(res2.a == "HOLA")
  }

  it should " let us map our own types (Own) with a custom anonymous map function" in {

    case class MyCaseClass[A, B](a:A, b:B)
    case class MyContainer[A](a:A)

    val localCaseClass = MyCaseClass[String, Int]("Uno", 1)
    val localContainer = MyContainer[MyCaseClass[String, Int]]( a = localCaseClass )

    implicit val myCaseClassFunctor = new Functor[MyContainer]{
      def map[A,B](fa: MyContainer[A])(f:A=>B) = MyContainer(f(fa.a))
    }

    val F = Functor[MyContainer]

    val res1 = F.map(localContainer){caseclass=>caseclass.b * 2}
    val res2 = F.map(localContainer)(caseclass=>"HOLA")

    assert(res1.a == 2)
    assert(res2.a == "HOLA")
  }

  it should "behave the same as with a List of one element" in {

    case class MyCaseClass[A, B](a:A, b:B)
    case class MyContainer[A](a:A)

    val localCaseClass = MyCaseClass[String, Int]("Uno", 1)
    val localContainer = MyContainer[MyCaseClass[String, Int]]( a = localCaseClass )

    implicit val myCaseClassFunctor = new Functor[MyContainer]{
      def map[A,B](fa: MyContainer[A])(f:A=>B) = MyContainer(f(fa.a))
    }

    val F = Functor[MyContainer]

    def f(x:MyCaseClass[String, Int]): Int = {
      0
    }

    val res0 = F.map(localContainer)(f)

    println(s"res0: $res0")

    val list = List(localCaseClass)
    val mappedList = list.map( x => 0 )
    assert(res0.a == mappedList.head)

  }

  it should " let us replace a value with operation as" in {
    // Here is a demo for "as"
    // The use of this operator generates value with more complex structures
    // so the value gets replaces preserving the original structure of the functor.

    case class MyCaseClass[A, B](a:A, b:B)
    case class MyContainer[A](a:A)

    implicit val myCaseClassFunctor = new Functor[MyContainer]{
      def map[A,B](fa: MyContainer[A])(f:A=>B) = MyContainer(f(fa.a))
    }

    val localCaseClass = MyCaseClass[String, Int]("Uno", 1)
    val localContainer = MyContainer[MyCaseClass[String, Int]]( a = localCaseClass )

    val res1 = localContainer.as(123)
    println(s"res1: $res1")
    assert(res1.a == 123)

  }

  it should " work well with fpoint" in {
    // Here is a demo for "as"
    // The use of this operator generates value with more complex structures
    // so the value gets replaces preserving the original structure of the functor.

    case class MyCaseClass[A, B](a:A, b:B)
    case class MyContainer[A](a:A)

    implicit val myCaseClassFunctor = new Functor[MyContainer]{
      def map[A,B](fa: MyContainer[A])(f:A=>B) = MyContainer(f(fa.a))
    }

    val localCaseClass = MyCaseClass[String, Int]("Uno", 1)
    val localContainer = MyContainer[MyCaseClass[String, Int]]( a = localCaseClass )

    val res2 = localContainer.fpoint[List]
    println(s"res2: $res2")

    assert(true)

  }

}
