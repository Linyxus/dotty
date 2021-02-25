
object Test {
  def foo[T]: {type X = T} => T = {
    case _: {type Y >: Int; type X = Y} => 0
  }
}
