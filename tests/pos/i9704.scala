abstract class WordSpec {
  export this.{extension_in => extension_should}

  extension(s: String) def in(f: => Any): Unit = {
    println(s)
    f
  }
}

object test extends WordSpec {
  "X" should {
    "add numbers" in {
     assert(1 + 1 == 2)
    }
  }
}
