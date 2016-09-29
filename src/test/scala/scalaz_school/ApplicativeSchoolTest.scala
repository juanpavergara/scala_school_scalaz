package scalaz_school

import org.scalatest.FunSuite
import scalaz._, Scalaz._

class ApplicativeSchoolTest extends FunSuite{

  test("Un Applicative Functor debe funcionar como un Functor con ap con funcion de aridad 1"){

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

  test("Un Applicative Functor debe poder aplicar funciones de aridad n>1"){

    case class MyGADT[A](a:A)

    implicit val myCaseClassApplicative = new Applicative[MyGADT]{
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
//    lo cual le permite comenzar a comprender que siempre estamos hablando de endofunctores (transformaciones de una categoria a la misma
//    es decir de MyGADT a MyGADT.
    val res1: MyGADT[String] = Appl.ap(a)(Appl.ap(b)(f1))
    val res2: MyGADT[String] =  a <*> (b <*> f1)

    assert(res1 == res2)
    res1.map(x=>res2.map(y=>assert(x==y)))

  }

}
