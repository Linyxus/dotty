object Test {
  final class Tag[T]

  def foo[T](x : { type X >: Tag[Int] <: Tag[T] }): T = x match {
    case _ : { } => 0
  }

}
