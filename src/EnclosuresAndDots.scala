
object EnclosuresAndDots {
  import Formatting._

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
  case class Pure(idiom: Idiom, of: Node) extends Node
  case class AntiPure(idiom: Idiom) extends Node

  case class IdiomKind(pure: Node, ap: Node, join: Option[Node], predict: Option[Node])
  case class Idiom(index: Int, kind: IdiomKind)

  class Enclosure(index: Int, kind: IdiomKind) {
    def p(k: IdiomKind)(f: (Enclosure) => Node) = Pure(Idiom(index, k), f(new Enclosure(index + 1, k)))
    val a: Node = AntiPure(Idiom(index, kind))
  }

  val K = App(↘ :: ○ :: Nil, I, I)
  val S = App(↙ :: ↘ :: ↡ :: Nil, I, I)

  val nada = IdiomKind(I, I, None, None)
  val lam = IdiomKind(K, S, None, None)

  object base {
    def p(k: IdiomKind)(f: Enclosure => Node) = Pure(Idiom(0, k), f(new Enclosure(1, k)))
  }

  sealed trait Dot
  case object ↙ extends Dot
  case object ↘ extends Dot
  case object ↡ extends Dot
  case object ○ extends Dot

  trait Reductions { self: Node =>
    lazy val reducedOnce: Option[Node] = carReducedOnce orElse locallyReducedOnce

    lazy val carReducedOnce: Option[Node] = this match {
      case App(dots, car, cdr) => car.reducedOnce map (App(dots, _, cdr))
      case _ => None
    }

    lazy val cdrReduced: Node = this match {
      case App(dots, car, cdr) => App(dots, car, cdr.reduced)
      case _ => this
    }

    lazy val locallyReducedOnce: Option[Node] = ({
      case App(Nil, I, x) => x
      case App(Nil, App(dot::dots, car, cdr), x) => dot match {
        case ↙ => App(dots, car(x), cdr)
        case ↘ => App(dots, car, cdr(x))
        case ↡ => App(dots, car(x), cdr(x))
        case ○ => App(dots, car, cdr)
      }
    }: PartialFunction[Node,Node]).lift(this)

    lazy val reduced: Node = (carReducedOnce
      orElse locallyReducedOnce
      map (_.reduced)
      getOrElse this
    ).cdrReduced

    lazy val extracted = this.childrenExtracted.locallyExtracted

    lazy val childrenExtracted = this match {
      case App(dots, car, cdr) => App(dots, car.extracted, cdr.extracted)
      case Pure(idiom, of) => Pure(idiom, of.extracted)
      case other => other
    }

    lazy val locallyExtracted = this match {
      // The basic cases.
      case App(Nil, App(Nil, AntiPure(idiom1), x), App(Nil, AntiPure(idiom2), y)) =>
        if (idiom1.index == idiom2.index) {
          assert(idiom1 == idiom2)
          ???
        }
        else if (idiom1.index < idiom2.index) {
          ???
        }
        else {
          ???
        }
      case App(Nil, App(Nil, AntiPure(idiom), x), y) => ???
      case App(Nil, x, App(Nil, AntiPure(idiom), y)) => ???

      // TODO: The "complicated" cases.

      // TODO: The unhappy cases.
    }
  }

  object Formatting {
    def fmtDots(dots: List[Dot]): String = (dots map (_.toString)).mkString
  }
}
