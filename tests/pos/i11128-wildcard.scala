package foo

object Outer {

  object Wrap {
    export Outer._
  }

  class Bar

  class Qux(i: Int)
  object Qux

}

import Outer._

val wrapBar = Wrap.Bar()

val wrapQux = Wrap.Qux(23)
