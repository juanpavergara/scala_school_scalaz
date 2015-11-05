package scalaz_school

import scalaz._


object main extends App {

  type Stack = List[Int]

  val pop = State[Stack, Int] {
    case x :: xs => (xs, x)
  }

  def push(a: Int) = State[Stack, Unit] {
    case xs => (a :: xs, ())
  }

  def stackManip: State[Stack, Int] = for {
    _ <- push(3)
    a <- pop
    b <- pop
  } yield(b)

  val emptyStack: State[Stack, Unit] = State.init.flatMap { stack =>
    if(stack.isEmpty) {
      State.state( () )
    } else {
      for {
        a <- pop
        _ <- emptyStack
      } yield ()
    }
  }

  val res: (Stack, Int) = stackManip(List(5, 8, 2, 1))
  println(s"Resultado de stackManip: $res")

  val resEmpty: (Stack, Unit) = emptyStack(List(1,2,3,4,5,6))
  println(s"Resultado de emptyStack: $resEmpty")

}
