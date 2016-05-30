package scalaz_school

import org.scalatest.FunSuite
import scalaz._


class ReaderSchoolTest extends FunSuite{

  test("Smoke test"){
    assert(true)
  }

  test("Reader as an input for a function"){

//    Lo interesante de Reader es que separa claramente lo que
//    es parámetros de la función, y su configuración
//    Para myName 'step' es un parametro necesario para la funcion
//    mientras que hay otra entrada que no es un parámetro
//    pero que influye en el cómputo. A este se le denomina 'configuración'
//    y se le inyecta mediante el Reader

    def myName(step: String): Reader[String, String] = Reader {
      step + ", I am " + _
    }

    val r1: Reader[String, String] = myName("Hello")

    // Aqui tienes dos formas de ejecutar un reader que ya esta montado
    val r2 = r1("JP!")
    val r3 = r1.run("JP!")

    // Fijate como son iguales con las dos maneras de ejecutar
    assert(r2 == r3)

    // Y aqui ves como lo que se pasó como input a MyName es agregado al resutlado
    assert(r2 == "Hello, I am JP!")

  }

  test("Nesting Readers of the same type"){

    def myName(step: String): Reader[String, String] = Reader {
      step + ", I am " + _
    }

    def localExample: Reader[String, (String, String, String)] = for {
      a <- myName("First")
      b <- myName("Second") >=> Reader { _ + "dy"}
      c <- myName("Third")
    } yield (a, b, c)

    def result: (String, String, String) = localExample("Fred")

    println(result)

    assert(result == ("First, I am Fred","Second, I am Freddy","Third, I am Fred"))

  }

  test("Stacking some monads on Reader"){

    type ReaderTOption[A, B] = ReaderT[Option, A, B]

    object ReaderTOption extends KleisliInstances with KleisliFunctions {
      def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = kleisli(f)
    }

    def configure(key: String) = ReaderTOption[Map[String, String], String] {
      _.get(key)
    }

    def setupConnection: ReaderT[Option, Map[String, String], (String, String, String)] = for {
      host <- configure("host")
      user <- configure("user")
      password <- configure("password")
    } yield (host, user, password)

    val goodConfig = Map(
      "host" -> "eed3si9n.com",
      "user" -> "sa",
      "password" -> "****"
    )

    val result = setupConnection(goodConfig)

    println(result)

//    result.foreach(x => {
//      assert(x._1 == "eed3si9n.com")
//      assert(x._2 == "sa")
//      assert(x._3 == "****")
//    })

  }

}
