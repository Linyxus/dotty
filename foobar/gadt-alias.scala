object Test {

  trait Expr[T]
  class Const[T] extends Expr[T]

  def foo[T](x: Expr[T]): T = {
    val e = new Const[Int]

    type X = T

    e match {
      case _ : Expr[X] => 0
    }
  }
}
