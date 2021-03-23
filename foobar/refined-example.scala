object Test {

  // def foo(x : { type X <: Int }, a : x.X) = {
  //   val i : Int = a
  // }

  def bar[T](x : { type X <: T; type S >: T }, a : x.X) = {
    val s : x.S = a
  }

}
