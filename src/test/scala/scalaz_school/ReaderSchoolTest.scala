package scalaz_school

import org.scalatest.FunSuite
import scalaz._
import Scalaz._

/*
Esta suite esta basada en la explicacion de Monad Transformes de
Eugene Yokota que se encuentra en
http://eed3si9n.com/learning-scalaz/Monad+transformers.html
Se adicionan alguno test adicionales para completar el ciclo de entendimiento
y entrenamiento en Reader y ReaderT
 */
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
    val r2: Id.Id[String] = r1("JP!")
    val r3: Id.Id[String] = r1.run("JP!")

    // Fijate como son iguales con las dos maneras de ejecutar
    assert(r2 == r3)

    // Y aqui ves como lo que se pasó como input a MyName es agregado al resutlado
    assert(r2 == "Hello, I am JP!")

  }

  test("Nesting Readers of the same type"){

    //     Este ejemplo puede ser confuso porque el placeholder _
    //     hace las veces de aquello que será inyectado como ambiente
    //     mediante el Reader Monad :) ... En realidad no hace las veces
    //     de configuracion pero si se estudia se entiende el sentido
    def myName(step: String): Reader[String, String] = Reader {
      step + ", I am " + _
    }

    //    Esta escritura de la funcion myName puede hacer más explicito
    //    la existencia de la configuracion como aquello que está pendiente
    //    por ser inyectado como ambiente mediante la Reader Monad
    def myName2(step: String): Reader[String, String] = Reader {
      case conf:String => step + ", I am " + conf
    }

    def localExample: Reader[String, (String, String, String)] = for {
      a <- myName("First")
      b <- myName2("Second") >=> Reader { _ + "dy"}
      c <- myName("Third")
    } yield (a, b, c)

    def result: (String, String, String) = localExample("Fred")

    println(result)

    assert(result == ("First, I am Fred","Second, I am Freddy","Third, I am Fred"))

  }

  test("Stacking some monads on Reader (Good config)"){


    def configure(key: String) = ReaderT[Option, Map[String, String], String] {
      case m => m.get(key)
    }

    def setupConnection = for {
      host <- configure("host")
      user <- configure("user")
      password <- configure("password")
    } yield (host, user, password)

    val goodConfig = Map(
      "host" -> "eed3si9n.com",
      "user" -> "sa",
      "password" -> "****"
    )

    val result: Option[(String, String, String)] = setupConnection(goodConfig)

    result.foreach(
      x => assert(x._1=="eed3si9n.com")
    )
  }

  test("Stacking some monads on Reader (Bad config)"){

    def configure(key: String) = ReaderT[Option, Map[String, String], String] {
    //def configure(key: String) = ReaderTOption[Map[String, String], String] {
      case m => m.get(key)
    }

    def setupConnection = for {
      host <- configure("host")
      user <- configure("user")
      password <- configure("password")
    } yield (host, user, password)

    val badConfiguration = Map(
      "host" -> "eed3si9n.com",
      "user" -> "sa"
    )

    val result: Option[(String, String, String)] = setupConnection(badConfiguration)

    assert(!result.isDefined)
  }

  test("Try to stack without ReaderT"){


    def myNameSome: Reader[Any, Option[String]] = Reader {
      case x:String => Some(x)
    }

    def myNameNone: Reader[Any, Option[String]] = Reader {
      case x:String => None
    }

    /*
     * Este for comp evalúa todas las expresiones, sin importar que myNameNone
     * evalúe a None. Esto se debe al contexto Reader en el que se encuentra envuelta
     * la función.
     */
    def localExample = for {
      a <- myNameSome
      b <- myNameNone
      c <- myNameSome
    } yield (a, b, c)

    def result = localExample("JP")

    println(result)

    assert(result._1.isDefined)
    assert(!result._2.isDefined)
    assert(result._3.isDefined)
  }

  test("Option with for comp"){


    def myNameSome = {
      Some(1)
    }

    def myNameNone = {
      None
    }

    /*
    Este for comp evalua a un None porque myNameNone evalua a None
    y así debe funcionar Option.
    Es diferente a como funciona el test "Try to stack without ReaderT"
    en el que el computo no se detiene cuando la función que envuelve el Reader
    retorna None. Si se quieren las funcionalidades de las dos Monadas
    entonces se deben apilar con ReaderT :)
     */
    def localExample = for {
      a <- myNameSome
      b <- myNameNone
      c <- myNameSome
    } yield (a, b, c)

    def result = localExample

    println(result)
    assert(!result.isDefined)
  }

  test("When stacking monads is not necessary :)"){

    def configure1(key: String) = ReaderT[Option, Map[String, String], String] {
      case m => m.get(key)
    }

    def configure2(key:String) = Reader[Map[String, String], Option[String]] {
      case m => m.get(key)
    }


    val goodConfig = Map(
      "host" -> "eed3si9n.com",
      "user" -> "sa",
      "password" -> "****"
    )

    /*
    Si vas a usar ReaderT es para apilar monadas
    lo que quiere decir que vas a necesitar componer computos
    con la monada que apilas con Reader.
    En este caso no estas componiendo el Option apilado con Reader
    asi que de nada te sirvio hacer un ReaderT
    */
    val result1 = configure1("host")(goodConfig)

    /*
    Fijate como este result2 es igual a result1 y no hubo necesidad de apilar
    monadas.
     */
    val result2 = configure2("host")(goodConfig)

    assert(result1.isDefined)
    assert(result1.getOrElse("X") == "eed3si9n.com")
    /*
    Esta es la prueba reina de que si no se necesita apilar no hay que hacer ReaderT
    Asi Reader sea un ReaderT con la monada identidad.
     */
    assert(result1.getOrElse("X") == result2.getOrElse("X"))
  }

}
