object RecursiveMethod {
  import scala.annotation.internal.local

  class SFile

  class Class(f: SFile) {
    def count(i: Int): SFile =
      if (i == 0) f else count(i - 1)
  }

  def withFile[T](thunk: (SFile @local) => T): T = thunk(new SFile)

  withFile { f =>
    val v = new Class(f)

    v.count(5) // error
  }
}