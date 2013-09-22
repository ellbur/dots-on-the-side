
/*
 * Here we demonstrate dots, which are a convenience, not really a theoretical
 * wonder. But, man, are they convenient.
 */
object Dots extends App {
  trait Node extends Reductions {
    def apply(cdr: Node) = App(Nil, this, cdr)
  }

  case object I extends Node
  case object X extends Node
  case object Y extends Node
  case class App(dots: List[Dot], car: Node, cdr: Node) extends Node {
    override def toString = dots match {
      case Nil => s"$car($cdr)"
      case dots => s"$car[${fmtDots(dots)}]($cdr)"
    }
  }

  def fmtDots(dots: List[Dot]): String = (dots map (_.toString)).mkString

  sealed trait Dot
  case object ↙ extends Dot
  case object ↘ extends Dot
  case object ↡ extends Dot
  case object ○ extends Dot

  trait Reductions { self: Node =>
    lazy val reduced: Option[Node] = ({
      case App(Nil, I, x) => x
      case App(Nil, App(dot::dots, car, cdr), x) => dot match {
        case ↙ => App(dots, car(x), cdr)
        case ↘ => App(dots, car, cdr(x))
        case ↡ => App(dots, car(x), cdr(x))
        case ○ => App(dots, car, cdr)
      }
    }: PartialFunction[Node,Node]).lift(this)

    lazy val moreReduced: Option[Node] = this match {
      case App(dots, car, cdr) => car.moreReduced match {
        case Some(carR) => Some(App(dots, carR, cdr).allReduced)
        case None => this.reduced match {
          case Some(r) => Some(r.allReduced)
          case None => None
        }
      }
      case _ => None
    }

    lazy val allReduced: Node = moreReduced getOrElse this match {
      case App(dots, car, cdr) => App(dots, car, cdr.allReduced)
      case other => other
    }
  }

  val f = App(↙ :: ↘ :: Nil, I, I)

  print("f = ")
  println {
    f.allReduced
  }

  println("f(x)(y) = ")
  println {
    f(X)(Y).allReduced
  }

  val K = App(↘ :: ○ :: Nil, I, I)
  print("K(X)(Y) = ")
  println(K(X)(Y).allReduced)
}
