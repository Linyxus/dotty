class A:
  var varInt: Int = 1
  def varInt_= : Unit = println("ok")

class B:
  val a = new A
  export a.varInt
  export a.varInt_=
  varInt = 2
