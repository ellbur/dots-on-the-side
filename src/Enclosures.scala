
object Enclosures {
  sealed trait Node {
    def apply(cdr: Node) = App(this, cdr)
  }

  case object S extends Node
  case object K extends Node
  case object I extends Node
  case class App(car: Node, cdr: Node) extends Node

  case class IdiomKind(pure: Node, ap: Node, join: Option[Node], predict: Option[Node])
  case class Idiom(index: Int, kind: IdiomKind)
  case class Pure(idiom: Idiom, of: Node) extends Node
  case class AntiPure(idiom: Idiom) extends Node

  class Enclosure(index: Int, kind: IdiomKind) {
    def p(k: IdiomKind)(f: (Enclosure) => Node) = Pure(Idiom(index, k), f(new Enclosure(index + 1, k)))
    val a: Node = AntiPure(Idiom(index, kind))
  }

  val nada = IdiomKind(I, I, None, None)
  val lam = IdiomKind(K, S, None, None)

  val base = new Enclosure(0, nada)

  // Basic eta-expanded identity.
  val etaId = base.p(lam) { x =>
    x.a(I)
  }

  // That x(y) thing.
  val xy = base.p(lam) { x =>
    x.p(lam) { y =>
      x.a(I)(y.a(I))
    }
  }

  // The strip-put-back-thing aka flip.
  val flip = base.p(lam) { x =>
    x.p(lam) { y =>
      x.a(y.a(I))
    }
  }
}
