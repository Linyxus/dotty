package example

trait Serial[A] {
  type SerialVersionId <: Long
}

object Serial {

  given Serial[Int] with {
    type SerialVersionId <: Long
  }

  transparent inline def derived[A](using deriving.Mirror.Of[A]): Serial[A] = new { type SerialVersionId = 12l }
}

case class Foo[T](t: T) derives Serial

val m = Foo.derived$Serial[Int] // TODO `T` is not instantiated.
