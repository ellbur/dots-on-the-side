
/*
 * Here we demonstrate dots, which are a convenience, not really a theoretical
 * wonder. But, man, are they convenient.
 */
object Dots extends App {
  trait Node {
    def apply(cdr: Node) = App(Nil, this, cdr)
  }

  case object I extends Node
  case object X extends Node
  case object Y extends Node
  case class App(dots: List[Dot], car: Node, cdr: Node) extends Node

  sealed trait Dot
  case object Left extends Dot
  case object Right extends Dot
  case object Both extends Dot
  case object Neither extends Dot

  def reduce(n: Node): Option[Node] = ({
    case App(Nil, I, x) => x
    case App(Nil, App(top :: rest, car, cdr), x) => top match {
      case Left => App(rest, car(x), cdr)
      case Right => App(rest, car, cdr(x))
      case Both => App(rest, car(x), cdr(x))
      case Neither => App(rest, car, cdr)
    }
  }: PartialFunction[Node,Node]).lift(n)

  def reduceAll(n: Node): Node = reduce(n) match {
    case None => n
    case Some(n2) => reduceAll(n2)
  }

  val f = App(Left :: Right :: Nil, I, I)

  print("f = ")
  println {
    reduceAll {
      f
    }
  }

  println("f(x)(y) = ")
  println {
    reduceAll {
      f(X)(Y)
    }
  }
}
