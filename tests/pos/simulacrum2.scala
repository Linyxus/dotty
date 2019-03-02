import simulacrum._

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
@typeclass trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: => A): F[A]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(pure(f))
}
@typeclass trait Monad[F[_]] extends Applicative[F] {
  // @op(">>=", alias = true) def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  @op(">>=") def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] =
    flatMap(f)(map(fa))
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))
}
@typeclass trait PlusEmpty[F[_]] {
  def empty[A]: F[A]
}
@typeclass trait MonadPlus[F[_]] extends Monad[F] with PlusEmpty[F] {
  self =>
  class WithFilter[A](fa: F[A], p: A => Boolean) {
    def map[B](f: A => B): F[B] = self.map(filter(fa)(p))(f)
    def flatMap[B](f: A => F[B]): F[B] = self.flatMap(filter(fa)(p))(f)
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](fa, x => p(x) && q(x))
  }

  def withFilter[A](fa: F[A])(p: A => Boolean): WithFilter[A] = new WithFilter[A](fa, p)
  def filter[A](fa: F[A])(f: A => Boolean) =
    flatMap(fa)(a => if (f(a)) pure(a) else empty[A])
}

sealed trait Maybe[+A]
case class Just[A](value: A) extends Maybe[A]
case object Empty extends Maybe[Nothing]
object Maybe {

  def just[A](a: A): Maybe[A] = Just(a)
  def empty[A]: Maybe[A] = Empty

  implicit val instance: MonadPlus[Maybe] = new MonadPlus[Maybe] {
    def pure[A](a: => A) = just(a)
    def empty[A] = Maybe.empty[A]
    def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]) = fa match {
      case Just(a) => f(a)
      case e @ Empty => Empty
    }
  }
}

object Test {
  import MonadPlus.ops._

  def main(args: Array[String]): Unit = {
    // We get the map function from Functor.Ops, which is the super-super-super class of MonadPlus.Ops
    assert(Maybe.just(1).map((_: Int) + 1) == Maybe.just(2))

    // We get >>= syntax as an alias for flatMap from the super-class of MonadPlus.Ops
    val recriprocal: Int => Maybe[Double] = x => if (x == 0) Maybe.empty else Maybe.just(1.0 / x)
    Maybe.just(1) >>= recriprocal

    // We get map from Functor.Ops, flatMap from Monad.Ops, and filter from MonadPlus.Ops
    // def div(x: Maybe[Int], y: Maybe[Int]): Maybe[Double] = for {
    //   xx <- x
    //   yy <- y
    //   if (yy > 0)
    //     } yield xx.toDouble / yy

    // assert(div(Maybe.just(1), Maybe.just(2)) == Maybe.just(1.toDouble / 2))
    // assert(div(Maybe.just(1), Maybe.empty) == Maybe.empty)
  }
}
