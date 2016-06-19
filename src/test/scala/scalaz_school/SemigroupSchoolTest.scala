package scalaz_school

import org.scalatest._
import scalaz._
import scalaz.Scalaz._
import Scalaz._

class SemigroupSchoolTest extends FunSuite{

  test("Sumar dos ADT") {

    sealed case class Cobertura(nombre:String)

    object miSemigrupo{
      implicit object miSemigrupoSemi extends Semigroup[Cobertura]{
        override def append(a:Cobertura, b: =>Cobertura): Cobertura = Cobertura(a.nombre.head + b.nombre.tail)
      }
    }

    import miSemigrupo._

    val a = Cobertura("mesto sera desechado de mi nombre")
    val b = Cobertura(".i primera palabra ahora esta completa")
    val c = a |+| b
    println(c)    
    assert(c === Cobertura("mi primera palabra ahora esta completa"))
  }
  
    
}