class ArrayOrdering[N] extends Comparable[Array[N]] {
  override def compareTo(x: Array[N]): Int = 0
}

class ArrayIntOrdering extends Comparable[Array[Int]] {
  override def compareTo(x: Array[Int]): Int = 0
}

class ArrayOrdering2[N] extends Comparable[Array[N]] {
  self: Serializable =>
  override def compareTo(x: Array[N]): Int = 0
}

class ArrayIntOrdering2 extends Comparable[Array[Int]] {
  self: Serializable =>

  override def compareTo(x: Array[Int]): Int = 0
}
