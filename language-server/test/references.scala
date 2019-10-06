import scala.quoted._

private object MacroReference {
  inline def hi(expr: => Any) <: Any =
    ${ hiImpl('expr) }

  def hiImpl(expr: Expr[Any])(implicit qctx: QuoteContext) = {
    import qctx.tasty._

    setReferences(List(expr.unseal.pos))

    expr
  }

  def test = {
    hi(1234)
  }
}