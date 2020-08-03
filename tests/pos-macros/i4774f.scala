
import scala.quoted._

object Test {
  def loop[T](x: Expr[T])(implicit t: Staged[T], qctx: QuoteContext): Expr[T] =
    '{ def y: T = $x; ${ loop('y) } }

  def loop2[T](x: Expr[T])(implicit t: Staged[T], qctx: QuoteContext): Expr[T] =
    '{ def y(): T = $x; ${ loop2('{y()}) } }
}
