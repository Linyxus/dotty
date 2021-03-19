
object Test {
  final class Tag[T]

  // def func0[Z >: Int <: Int, Y >: Z <: Z, X >: Y <: Y]: X = {
  //   0
  // }

  // def func0_[Z >: Int <: Int, Y >: Z <: Z, X >: Y <: Y, T >: X <: X, S]: Tag[S] => S = {
  //   case _ : Tag[T] => 0
  // }

  // def func1[Z >: Int <: Int, Y >: Z <: Z, T >: Y <: Y]: T = {
  //   0
  // }

  // // works: Int = Z = Y = T
  // def func2[Z >: Int <: Int, Y >: Z <: Z, T]: Tag[T] => T = {
  //   case _ : Tag[Y] => 0
  // }

  // equation chain: Int = Z = Y = X = T
  // def func3[Z >: Int <: Int, Y >: Z <: Z, X >: Y <: Y, T >: X <: X]: T = {
  //   0
  // }

  // However, when the 'equation chain' is longer
  // expected: Int = Z = Y = X = T
  // but: can not derive T = Int
  // why?
  def func4[Z >: Int <: Int, Y >: Z <: Z, X >: Y <: Y, T]: Tag[T] => T = {
    case _ : Tag[X] => 0
  }

  // def func5[X0 >: Int <: Int, X1 >: X0 <: X0, X2 >: X1 <: X1, X3 >: X2 <: X2, T]: Tag[T] => T = {
  //   case _ : Tag[X1] => 0
  // }
}
