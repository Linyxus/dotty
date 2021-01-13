class Test {

  sealed trait Parent
  case class Foo(x: Int, y: Int, s: String) extends Parent
  case class Bar(x: Int, y: Int) extends Parent

  12.dskflksdflkds // error: anonymous Mirror generated.
}

class Test2 {
  println(summon[deriving.Mirror.Of[List[Int]]]) // ok: List is Scala 2 defined
}
