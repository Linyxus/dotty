object Test {
  trait Tag[T]
  class IntTag extends Tag[Int]
  final class InvCov[A, +B]
  final class InvInv[A, B]

  // def fooGood[X, T](x: InvInv[X, X]) = x match {
  //   case _ : InvInv[IntTag, Tag[T]] =>
  //     val t : T = 0
  // }

  // def fooBad[X, T](x: InvInv[X, Tag[T]]) = x match {
  //   case _ : InvInv[Tag[Int], X] =>
  //     val t : T = 0
  // }

  // def bad[X](x: Int): X = x match {
  //   case _ : X => 0
  // }

  // def baz[X, T](x: InvCov[X, X]) = x match {
  //   case _ : InvCov[IntTag, Tag[T]] =>
  //     val t : T = ???
  //     val x : Int = t
  // }

  // def bar[T](x : { type X = IntTag; type Y >: X <: Tag[T] }): T = x match {
  //   case _ : { } => 0
  // }

  // def bar[T](x : { type X = Tag[Int]; type Y >: Tag[T] <: X }) = x match {
  //   case _ : { } =>
  //     val t : T = ???
  //     val i : Int = t
  // }
}
