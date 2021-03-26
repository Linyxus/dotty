object Test {

  enum KSUB[-F[_], +G[_]] {
    case Refl[S[_]]() extends KSUB[S, S]
  }

  def foo[F[_], s[_]](
    sub: { type X <: F KSUB Option }
  ): Option[Int] =
    sub match {
      case _ : { type X = KSUB.Refl[s] } =>
        val fi : F[Int] = ???
        fi
    }

  enum SUB[T, U] {
    case Refl[S]() extends SUB[S, S]
  }

  def bar[F[_], G[_], s[_]](
    ksub: { type M <: F KSUB G },
  ) =
    ksub match {
      case _ : { type M = KSUB.Refl[s] } =>
        val fi : F[Int] = ???
        val si : s[Int] = fi  // error
        val gi : G[Int] = si  // error
    }
}
