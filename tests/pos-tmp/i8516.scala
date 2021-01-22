abstract class Fn1[-A, +B] {
  def apply(v1: A, x: Int): B
}
val x: Fn1[Int, Int] { def apply(arg: Int, i: Int): Int } = new Fn1[Int, Int] {
  def apply(arg: Int, i: Int): Int = arg
}
val x1 = x
val y = x.apply(arg = 1, i = 2)
