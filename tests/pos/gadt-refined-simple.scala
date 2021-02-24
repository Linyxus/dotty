
object Test {
  def foo[T]: {type X = T} => T = {
    case _: {type X = Int} => 0
  }
}
