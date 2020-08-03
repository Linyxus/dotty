import scala.quoted._
class Foo {
  def f[T2: Staged](e: Expr[T2])(using QuoteContext) = e match {
    case '{ $x: *:[Int, $t] } =>
      '[ *:[Int, $t] ]
  }
}
