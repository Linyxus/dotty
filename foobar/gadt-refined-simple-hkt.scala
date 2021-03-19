
object Test {
  class Foo[A]
  trait Tag[F[_]]
  class FooTag extends Tag[Foo]

  def foo[F[_]](x : { type X >: FooTag <: Tag[F] }) = x match {
    case _ : { } =>
      val z : F[Int] = ??? : X[Int]
  }
}
