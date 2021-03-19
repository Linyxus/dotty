object EQK {
  sealed trait EQ[A, B]
  final case class Refl[A]() extends EQ[A, A]

  sealed trait EQK[F[_], G[_]]
  final case class ReflK[F[_]]() extends EQK[F, F]

  final class InvInv[A, B]

  def m0[F[_], G[_], A](fa: F[A], eqk: { type X >: ReflK[F] <: EQK[F, G] }): G[A] = eqk match {
    case _ : { } => fa
  }

  def m1[F[_], G[_], A](fa: F[A], eq: { type M >: Refl[A] <: EQ[A, Int] }, eqk: { type N >: ReflK[F] <: EQK[F, G] }): G[Int] = eqk match {
    case _ : { } => eq match {
      case _ : { } =>
        val r1: F[Int] = fa
        val r2: G[A] = fa
        val r3: F[Int] = r2
        fa : G[Int]
    }
  }
}
