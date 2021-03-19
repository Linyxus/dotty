
object TestMain extends App {

  def foo: (Int => Unit, Unit => Int) = {
    var value: Int = 0
    def set(x: Int): Unit = value = x
    def get(x: Unit): Int = value

    return (set, get)
  }

  val (set, get) = foo

  println(get(()))
  println(set(10))
  println(get(()))

}
