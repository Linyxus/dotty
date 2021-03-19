
object Test {
  trait BiTag[A, B, +T]
  final class PrimBiTag[A, B, T] extends BiTag[A, B, T]

  def foo[X, Y, T](x: BiTag[Y, X, T]): T = x match {
    case _: PrimBiTag[Int, Y, X] => 0
  }
}
