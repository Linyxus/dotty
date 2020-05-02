object RecursiveMethod {
  import scala.annotation.internal.{local, entry}

  class SFile

  class Class(f: SFile) {
    def count(i: Int): Int =
      if (i == 0) 0 else count(i - 1)
  }

  def withFile[T](thunk: (SFile @local) => T): T = thunk(new SFile)

  @entry def foo() =
    withFile { f =>
      val v = new Class(f)

      v.count(5)
    }
}
