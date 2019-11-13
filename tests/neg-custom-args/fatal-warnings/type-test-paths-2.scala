import scala.tasty.TypeTest

trait R {
  type Nat
  type Succ <: Nat
  type Idx
  given TypeTest[Nat, Succ] = typeTestOfSucc
  protected def typeTestOfSucc: TypeTest[Nat, Succ]
  def n: Nat
  def one: Succ
}

object RI extends R {
  type Nat = Int
  type Succ = Int
  type Idx = Int
  protected def typeTestOfSucc: TypeTest[Nat, Succ] = new {
    def isInstance(x: Int): TypeTest.Result[x.type & Succ] =
      if x > 0 then TypeTest.success(x) else TypeTest.failure
  }
  def n: Nat = 4
  def one: Succ = 1
}

object Test {
  val r1: R = RI
  import r1.given

  val r2: R = RI
  import r2.given

  r1.n match {
    case n: r2.Nat => // error: the type test for Test.r2.Nat cannot be checked at runtime
    case n: r1.Idx => // error: the type test for Test.r1.Idx cannot be checked at runtime
    case n: r1.Succ => // Ok
    case n: r1.Nat => // Ok
  }

  r1.one match {
    case n: r2.Nat => // error: the type test for Test.r2.Nat cannot be checked at runtime
    case n: r1.Idx => // error: the type test for Test.r1.Idx cannot be checked at runtime
    case n: r1.Nat => // Ok
  }
}
