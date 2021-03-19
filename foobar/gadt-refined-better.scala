object test {

  def foo(x : { type X }): x.X = x match {
    case _ : { type X = Int } => 0
  }

}
