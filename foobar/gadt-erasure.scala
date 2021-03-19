trait TagA[A]
trait TagB[B]
trait BiTag[A, B] extends TagA[A] with TagB[B]
class IntStrTag extends TagA[Int] with TagB[String]

def biget[A, B]: TagA[Int] & TagB[String] => (A, B) = {
  case _: BiTag[A, B] => (0, "zero")
}

object GadtErasure extends App {
  val value = new IntStrTag with BiTag[Int, String]
  val ret = biget(value)
  println(ret)

  val unsound: (String, String) = biget[String, String](value)
  val x: String = unsound._1
  val y: Boolean = x.startsWith("hello")
  println(x)
  println(y)
}
