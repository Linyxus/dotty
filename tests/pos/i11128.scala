package foo

object Wrap {
  export foo.Bar
  // export foo.Qux
}

class Bar

val wrapBar = Wrap.Bar()

// class Qux(i: Int)
// object Qux

// val wrapQux = Wrap.Qux(23)
