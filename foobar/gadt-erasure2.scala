trait Tag[T]

def foo[T]: Tag[T] => T = {
  case _: Tag[Int] => 0
}

object GadtErasure2 extends App {
  val value = new Tag[String] {}
  val unsound = foo[String](value)
  unsound.startsWith("hello")
}
