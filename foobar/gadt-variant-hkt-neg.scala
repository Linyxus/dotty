object test {

  enum KSUB[-F[_], +G[_]] {
    case Refl[S[_]]() extends KSUB[S, S]
  }

  enum SUB[T, U] {
    case Refl[S]() extends SUB[S, S]
  }

  // def bar[F[_], G[_], X, S[_]](fx: F[X], ksub: F KSUB G, sub: X SUB Int) =
  //   ksub match {
  //     case _: KSUB.Refl[S] =>
  //       sub match {
  //         case SUB.Refl() =>
  //           val gi: G[Int] = fx : S[X]
  //           ()
  //       }
  //   }

  def baz[F[_], G[_], X](fx: F[X], ksub: F KSUB G, sub: X SUB Int) =
    ksub match {
      case _: KSUB.Refl[s] =>
        sub match {
          case SUB.Refl() =>
            val gi: G[Int] = fx : s[X]
            ()
        }
    }
}
