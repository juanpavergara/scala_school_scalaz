package scalaz_school

import org.scalatest.FunSuite
import scalaz._, Scalaz._
import scalaz.syntax.ApplicativeBuilder

class ApplicativeSchoolTest extends FunSuite{

  test("[TEORICO] Un Applicative Functor debe funcionar como un Functor con ap con funcion de aridad 1"){

//    Este case class es un type constructor.
//    Esto quiere decir que no es un tipo per se sino que cuando se construya un MyContainer
//    con un tipo especifico ahí si tendremos un tipo
    case class MyGADT[A](a:A)


//      A continuacion se hace del type constructor recien definido un Functor Aplicativo.
//      Esto en scalaz se hace instanciando Applicative y definiendo dos morfismos: point y ap
//      point levante un valor en el contexto
//      ap toma un valor en un contexto y una funcion en un contexto y le **aplica** la funcion al valor.
//      Fijese en el tipo de fa MyContainer[A=>B]
//      Aqui es muy importante recordar que con el tipo A=>B se puede denotar el tipo de una funcion de
//      aridad n. Esto se entiende mejor si recuerda dos cosas:
//      1. Que una funcion puede ser un tipo
//      2. Que en el calculo Lambda las funciones son de un solo parametro y se debe usar currying para hacer funciones
//      de aridad n>1

    implicit val myCaseClassApplicative = new Applicative[MyGADT]{
      def point[A](a: => A): MyGADT[A] = MyGADT(a)
      def ap[A,B](fa: => MyGADT[A])(f: => MyGADT[A => B]): MyGADT[B] = MyGADT(f.a(fa.a))
    }

//    Aqui creamos un nuevo Applicative con el container que ya definimos
//    Es importante ver que la construccion se hace a partir de Applicative de scalaz
//    y la decoración con point y ap se hace con el implicit val myCaseClassApplicative
    val Appl = Applicative[MyGADT]

//     f es una funcion de aridad 1
//     recuerde que una funcion de aridad n recibe n parametros
//     Esta funcion es la que será **aplicada** a un valor que se encuentre dentro del contexto MyContainer
//     Es intuitivo entonces que un Functor se llame Functor Aplicativo dado que el contexto contiene una funcion
//     a aplicar a otro valor dentro de un contexto de los mismos.
    val f = (i:Int) => i

//    Aqui lo que se hace es hacer varios Applicatives concretos con el que ya definimos.
//    Fijese que lo podemos hacer con la instanciacion del case class o con point
//    point lo puede encontrar en la literatura como pure
    val a = MyGADT(1)
    val d = MyGADT(f)
    val fa = Appl.point(f)

//    A continuacion puede ver dos sintaxis diferentes para aplicar una funcion (que esta en el contexto)
//    a un valor (que esta en el contexto)
//    Claramente la sintaxis <*> es mas legible que la de usar ap
//    La firma de <*> es ap(self)(f) así que es lo mismo
//    Esta aplicacion funciona igual que un Functor que no sea aplicativo
//    lo cual demuestra que todos los Applicative Functor son Functor y que para funciones de aridad 1
//    hacer ap es lo mismo que usar famp de Functor no applicativo
    val res1: MyGADT[Int] = Appl.ap(a)(fa)
    val res2: MyGADT[Int] =  a <*> d

//    Inicialmente verifiquemos que las sintaxis son equivalentes
//    verificando que dan el mismo tipo y contenido
    assert(res1 == res2)

//    Finalmente y siendo mas incredulos, nos metemos en los dos contextos y verificamos que los valores
//    sean iguales
    res1.map(x=>res2.map(y=>assert(x==y)))

  }

  test("[TEORICO] Un Applicative Functor debe poder aplicar funciones de aridad n>1"){

    case class MyGADT[A](a:A)

    implicit val myGADTApplicative = new Applicative[MyGADT]{
      def point[A](a: => A): MyGADT[A] = MyGADT(a)
      def ap[A,B](fa: => MyGADT[A])(f: => MyGADT[A => B]): MyGADT[B] = MyGADT(f.a(fa.a))
    }

    val Appl = Applicative[MyGADT]

//    Ahora f es una funcion de aridad n>1
//    Note lo siguiente:
//    1. No importan los tipos de los parametros. Pueden ser diferentes, como cualquier funcion normal
//    2. Que la funcion se define con varios parametros mediante currificacion. A es String y B es Int => String
    val f = (one:String) => ((two:Int) => one + two)

    val a = MyGADT(1)
    val b = MyGADT("A")
    val f1 = MyGADT(f)
    val f2 = Appl.point(f)

//    A continuacion se aplica la funcion a dos GADT que son diferentes. Note que a es 1 y b es "A"
//    Note también que el orden de la aplicacion es importante pues primero se aplica a b pues es quien contiene un String
//    y luego se aplica a a para aplicar el segundo parametro que es un Int
//    Finalmente note como la anotación de tipo le indica que el resultado de la aplicacion siempre va a resultar en un MyGADT
//    lo cual le permite comenzar a comprender que siempre estamos hablando de endofunctores (transformaciones de una categoria a la misma)
//    es decir de MyGADT a MyGADT.
    val res1: MyGADT[String] = Appl.ap(a)(Appl.ap(b)(f1))
    val res2: MyGADT[String] =  a <*> (b <*> f1)

    assert(res1 == res2)
    res1.map(x=>res2.map(y=>assert(x==y)))

  }

  test("La estructura de computo de un Applicative Functor debe ser fija"){
//    Estos estados mutables serviran solo para verificar que los computos en un Applicative se ejecutan siempre
//    y que en un Monad se ejecutan hasta el caso de falla de algun computo.
//    Cada que se ejecute un computo se incrementaran estas variables dando cuenta que se ha ejecutado la fn.
    var stateApplicative = 0
    var stateMonad = 0

//    Una funcion normal que evalua a Option.
//    Fijese que si se evalua se incrementan los estados de Monad y Applciative en 1
    def o1(i:Int) = {
      stateApplicative = stateApplicative + 1
      stateMonad = stateMonad + 1
      Option(i)
    }

    //    Una funcion normal que evalua a Option.
    //    Fijese que si se evalua se incrementan los estados de Monad y Applciative en 1
    def o2(i:Int) = {
      stateApplicative = stateApplicative + 1
      stateMonad = stateMonad + 1
      None
    }

//    Un GADT normal que estructura 3 Int
    case class MyGADTInts(a:Int, b:Int, c:Int)

//    Aqui esta la magia. Se ejecutan tres computos aplicativos.
//    Cuando se dice que la estructura de un aplicativo es fija quiere decir que todos los computos se ejecutaran
//    sin importar los resultados de cada uno. A diferencia de una monada los
//    applicative siempre ejecutan TODOS sus pasos.
//    El uso de scream (operador |@|) toma los resultados de cada computo y los utiliza para ejecutar la funcion currificada.
//    La funcion currificada en este caso simplemente es la construccion de un MyGADTInts
//    Es importante ver que |@| se pudo aplicar sobre Option **¿Por qué?**
//    Porque scalaz (cats tbn lo hace) ofrece conversiones implícitcas de Option a Applicative.
//    Sería interesante revisar si esto es posible también con cualquier otro F[]
    val resApplicative = ( o1(1) |@|
                           o2(2) |@|
                           o1(3))(MyGADTInts.apply)

//    Lo primero que debemos revisar es que todos los computos se hayan ejecutado, tal como lo promete
//    la estructura (o patron) Applicative
    assert(stateApplicative == 3)

//    Mire como, asi hayan sido ejecutados todos los computos, el resultado de la construccion del nuevo Applicative
//    evalua a None (porque o2(2) evalua a None)
    assert(resApplicative == None)

    stateMonad = 0
//    Ahora hagamos exactamente lo mismo pero con Option como Monad (no como Applicative).
//    Esto lo logramos con flatmap (ayudados con for-comp)
    val resMonad = for{
      a <- o1(1)
      b <- o2(2)
      c <- o1(3)
    } yield MyGADTInts(a,b,c)

//    Al igual que con Applicative, el resultado es None
    assert(resMonad == None)
//    Pero a diferencia de Applicative, con Monad solo se ejecutaron los computos hasta que se materializó
//    una tendencia negativa de la mónada que para el caso de Option es None.
    assert(stateMonad == 2)



  }


  test("Un Applicative debe permitir la aplicacion de funciones de aridad n > 1") {
    assert(true)
  }

  test("Un Applicative debe permitir traverse"){

    def f1(i:Int) = "1"
    def f2(i:Int) = "2"
    def f3(i:Int) = "3"

    def o1(i:Int) = {
      println(s"Ejecutado o1 con $i")
      Option(i)
    }

    def o2(i:Int) = {
      println(s"Ejecutado o2 con $i")
      None
    }

    case class MyGADT(a:String, b:String)

    val result: Option[List[Int]] = List(1, 2, 3) traverse { x => (x > 0) option (x + 1) }

    //TODO: Documentar que traverase siempre debe evaluar a G[] donde G es el type constructor que envuelve el Appl
    val result2 = Option(1) traverse { x => (x > 0) option (x + 1) }

    println(result)
    println(result2)

    assert(true)

  }
}
