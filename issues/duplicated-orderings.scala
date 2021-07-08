trait Expr[+T]
case class Lit[X](x: X) extends Expr[X]

def foo[B, D, A <: B, C <: D](m1: Expr[D], m2: Expr[C]) = m1 match {
  case _: Lit[A] =>
    m2 match {
      case _: Lit[B] =>
        ???
    }
}
