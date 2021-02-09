class ArrayOrdering[N] extends Comparable[Array[N]] {
  override def compareTo(x: Array[N]): Int = 0
}

class ArrayIntOrdering extends Comparable[Array[Int]] {
  override def compareTo(x: Array[Int]): Int = 0
}
