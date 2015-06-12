package scalaz_school

import org.scalatest.FlatSpec
import scalaz.Scalaz._

class SemigroupSchoolTest extends FlatSpec{

  "Semigroup " should " sumar dos coberturas" in {
    
    import miSemigrupo._
    val a = Cobertura("mesto sera desechado de mi nombre")
    val b = Cobertura(".i primera palabra ahora esta completa")
    val c = a |+| b  
    println(c)    
    assert(c === Cobertura("mi primera palabra ahora esta completa"))
  }
  
    
}