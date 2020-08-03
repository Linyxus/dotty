import scala.quoted._
object Macro {
  inline def ff[T: Staged](x: T): T = ${ impl('x) }
  def impl[T](x: Expr[T]): Expr[T] = x
}
