import scala.quoted._
def test(using QuoteContext) = {
  def f = {
    {
      '[String]
      '[String]
    }

    '[String] match { case _ => }
    try '[String] catch { case _ => }

    '[String]
    '[String]
  }

  def bar[T](t: quoted.Staged[T]) = ???
  bar('[String])

  class Baz[T](t: quoted.Staged[T])
  new Baz('[String])

}
