
object Test {
  def foo[T]: {type Y = T} => T = {
    case _: {type X = Int; type Y = X} => 0
  }

  def bar[T]: {type X <: T} => T = {
    case _: {type Y >: Int; type X >: Y} => 0
  }

  def baz[T]: {type X = T} => T = {
    case _: {type Z = Int; type Y = Z; type X = Y} => 0
  }

  def qux[T]: {type X = T} => T = {
    case _: {type Z = Int; type Y = Z; type X >: Y} => 0
  }

  def qux2[T]: {type Z = Int; type Y = Z; type X >: Y} => T = {
    case _: {type X = T} => 0
  }

  def quux[T]: {} => T = {
    case _: {type Z = Int; type Y = Z; type X >: Y <: T} => 0
  }

  def quuz[T]: {type Z = Int; type Y = Z; type X >: Y <: T} => T = {
    case _: {} => 0
  }
}

