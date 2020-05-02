object Method {
  import scala.annotation.internal.{local, entry}

  class SFile(path: String)

  def withFile[T](path: String)(@local thunk: (SFile @local) => T): T = {
    val f = new SFile(path)
    thunk(f)
  }

  class Class1(f: SFile) {
    def fileLength = 0
  }

  @entry def foo() = 
    withFile("") { f =>
      val c = new Class1(f)
      c.fileLength
    }

  class Class2(val f: SFile) {
    def fileLength = 0
  }

  @entry def bar() = 
    withFile("") { f =>
      val c = new Class2(f)
      c.fileLength
    }
}
