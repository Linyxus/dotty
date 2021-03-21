object test {
  final class HKTVar[+F[_]]

  // def foo[G[_], F[x] <: G[x], X](fx : F[X]) = {
  //   val gx : G[X] = fx
  // }

  def bar[F[_], G[_], X](x : HKTVar[G], fx : F[X]) = x match {
    case _ : HKTVar[F] =>
      val gx : G[X] = fx
  }

}
