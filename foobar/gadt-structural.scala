
def foo(t: { type Y }, x: { type X = t.Y }): t.Y =
  val t0: { type X = Int } = new { type X = Int }
  val i0: t0.X = 0
  new { type X <: t0.X } match {
    case x0: { type T = Int; type X = T } => 0
  }

