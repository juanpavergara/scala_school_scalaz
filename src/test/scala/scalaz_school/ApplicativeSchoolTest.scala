package scalaz_school

import org.scalatest.FunSuite
import scalaz._, Scalaz._

class ApplicativeSchoolTest extends FunSuite{

  test("Un Applicative Functor debe funcionar como un Functor con ap con funcion de aridad 1"){

//    Este case class es un type constructor.
//    Esto quiere decir que no es un tipo per se sino que cuando se construya un MyContainer
//    con un tipo especifico ahí si tendremos un tipo
    case class MyContainer[A](a:A)


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

    implicit val myCaseClassApplicative = new Applicative[MyContainer]{
      def point[A](a: => A): MyContainer[A] = MyContainer(a)
      def ap[A,B](fa: => MyContainer[A])(f: => MyContainer[A => B]): MyContainer[B] = MyContainer(f.a(fa.a))
    }

//    Aqui creamos un nuevo Applicative con el container que ya definimos
//    Es importante ver que la construccion se hace a partir de Applicative de scalaz
//    y la decoración con point y ap se hace con el implicit val myCaseClassApplicative
    val Appl = Applicative[MyContainer]

//     f es una funcion de aridad 1
//     recuerde que una funcion de aridad n recibe n parametros
//     Esta funcion es la que será **aplicada** a un valor que se encuentre dentro del contexto MyContainer
//     Es intuitivo entonces que un Functor se llame Functor Aplicativo dado que el contexto contiene una funcion
//     a aplicar a otro valor dentro de un contexto de los mismos.
    val f = (i:Int) => i

//    Aqui lo que se hace es hacer varios Applicatives concretos con el que ya definimos.
//    Fijese que lo podemos hacer con la instanciacion del case class o con point
//    point lo puede encontrar en la literatura como pure
    val a = MyContainer(1)
    val d = MyContainer(f)
    val fa = Appl.point(f)

//    A continuacion puede ver dos sintaxis diferentes para aplicar una funcion (que esta en el contexto)
//    a un valor (que esta en el contexto)
//    Claramente la sintaxis <*> es mas legible que la de usar ap
//    La firma de <*> es ap(self)(f) así que es lo mismo
//    Esta aplicacion funciona igual que un Functor que no sea aplicativo
//    lo cual demuestra que todos los Applicative Functor son Functor y que para funciones de aridad 1
//    hacer ap es lo mismo que usar famp de Functor no applicativo
    val res1: MyContainer[Int] = Appl.ap(Appl.ap(a)(fa))(fa)
    val res2: MyContainer[Int] =  a <*> d

//    Inicialmente verifiquemos que las sintaxis son equivalentes
//    verificando que dan el mismo tipo y contenido
    assert(res1 == res2)

//    Finalmente y siendo mas incredulos, nos metemos en los dos contextos y verificamos que los valores
//    sean iguales
    res1.map(x=>res2.map(y=>assert(x==y)))

  }


}
