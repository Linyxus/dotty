object Test {

  // def foo(x : { type X <: Int }, a : x.X) = {
  //   val i : Int = a
  // }

  def bar[T, S](x : { type X <: T; type S >: X }, a : x.X) = {
    val t : T = a
  }

}
