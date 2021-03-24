object test {
  final class HKTVar[+F[_]]
  final class HKTVarVar[+F[_], +M[_]]
  final class HKTInv[F[_]]

  // def foo[G[_], F[x] <: G[x], X](fx : F[X]) = {
  //   val gx : G[X] = fx
  // }

  // def bar[F[_], G[_], X](x : HKTVar[G], fx : F[X]) = x match {
  //   case _ : HKTVar[F] =>
  //     val gx : G[X] = fx
  // }

  // def baz[F[_], G[_], H[_], X](x : HKTVarVar[G, H], fx : F[X]) = x match {
  //   case _ : HKTVarVar[F, F] =>
  //     val gx : G[X] = fx
  // }

  def baaz[F[_], G[_], X](x : HKTInv[G], fx : F[X]) = x match {
    case _ : HKTInv[F] =>
      val gx : G[X] = fx
  }

}
