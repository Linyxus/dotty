trait Foo {
  def name: String

  val message = "hello, " + name
}

class Bar extends Foo {
  val name = "Jack"              // error: partial cannot be implemented by val
}


trait Zen {
  val name: String

  val message = "hello, " + name
}

class Tao extends Zen {
  val name = "Jack"              // error: init cannot be implemented by val
}


trait Base {
  val name: String

  val message = "hello, " + name
}

class Derived(val name: String) extends Base

class Derived2 extends Derived("hello") {
  override val name: String = "ok"
}
