package scalaz_school

import scalaz.Semigroup
import scalaz.Scalaz._

sealed case class Cobertura(nombre:String)

object miSemigrupo{
  implicit object miSemigrupoSemi extends Semigroup[Cobertura]{
    override def append(a:Cobertura, b: =>Cobertura): Cobertura = Cobertura(a.nombre.head + b.nombre.tail)
  }
}
