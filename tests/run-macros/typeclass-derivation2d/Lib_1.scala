import scala.collection.mutable
import scala.annotation.tailrec

import scala.quoted._

// Simulation of an alternative typeclass derivation scheme

// -- Classes and Objects of the Derivation Framework ----------------------------------

//** Core classes. In the current implementation these are in the scala.reflect package */
object Deriving {

  /** The Generic class hierarchy allows typelevel access to
   *  enums, case classes and objects, and their sealed parents.
   */
  sealed abstract class Mirror {

    /** The mirrored *-type */
    type _MonoType
  }

  object Mirror {

    /** The Mirror for a sum type */
    trait Sum extends Mirror { self =>

      type ElemTypes <: Tuple

      /** The ordinal number of the case class of `x`. For enums, `ordinal(x) == x.ordinal` */
      def ordinal(x: _MonoType): Int
    }

    /** The Mirror for a product type */
    trait Product extends Mirror {

      /** The types of the elements */
      type ElemTypes <: Tuple

      /** The name of the whole product type */
      type CaseLabel <: String

      /** The names of the product elements */
      type ElemLabels <: Tuple

      /** Create a new instance of type `T` with elements taken from product `p`. */
      def _fromProduct(p: scala.Product): _MonoType
    }

    trait Singleton extends Product {
      type _MonoType = this.type
      def _fromProduct(p: scala.Product) = this
    }
    type Of[T]        = Mirror { type _MonoType = T }
    type ProductOf[T] = Mirror.Product { type _MonoType = T }
    type SumOf[T]     = Mirror.Sum { type _MonoType = T }
   }

  /** Helper class to turn arrays into products */
  class ArrayProduct(val elems: Array[AnyRef]) extends Product {
    def this(size: Int) = this(new Array[AnyRef](size))
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
    def update(n: Int, x: Any) = elems(n) = x.asInstanceOf[AnyRef]
  }

  object EmptyProduct extends ArrayProduct(Array[AnyRef]())

  /** Helper method to select a product element */
  def productElement[T](x: Any, idx: Int) =
    x.asInstanceOf[Product].productElement(idx).asInstanceOf[T]
}
import Deriving._


// --------------- Equality typeclass ---------------------------------

trait Eq[T] {
  def eql(x: T, y: T): Boolean
}

object Eq {
  import scala.compiletime.{erasedValue, summonFrom}

  inline def tryEql[T](x: T, y: T) =
    ${ tryEqlExpr('x, 'y) }

  private def tryEqlExpr[T: Type](x: Expr[T], y: Expr[T])(using qctx: QuoteContext): Expr[Boolean] = {
    Expr.summon[Eq[T]] match {
      case Some(eq) => '{ $eq.eql($x, $y) }
      case None =>
        qctx.error(s"Could not find delegate for Eq[${summon[Type[T]].show}]")
        '{ false }
    }
  }

  inline def eqlElems[Elems <: Tuple](n: Int)(x: Any, y: Any): Boolean =
    ${ eqlElemsExpr[Elems]('n)('x, 'y) }

  private def eqlElemsExpr[Elems <: Tuple](n: Expr[Int])(x: Expr[Any], y: Expr[Any])(using Type[Elems])(using QuoteContext): Expr[Boolean] =
    '{ ??? : Elems } match {
      case '{ type $elems1 <: Tuple; $_ : ($elem *: `$elems1`) } =>
        '{
          ${ tryEqlExpr('{ productElement[$elem]($x, $n) }, '{ productElement[$elem]($y, $n) })(using elem, summon) } &&
          ${ eqlElemsExpr('{ $n + 1})(x, y)(using elems1) }
        }
      case '{ $_ : Unit } =>
        '{true}
    }

  inline def eqlProduct[T](m: Mirror.ProductOf[T])(x: Any, y: Any): Boolean =
    eqlElems[m.ElemTypes](0)(x, y)

  inline def eqlCases[Alts](inline n: Int)(x: Any, y: Any, ord: Int): Boolean =
    ${ eqlCasesExpr[Alts]('n)('x, 'y, 'ord) }

  def eqlCasesExpr[Alts](n: Expr[Int])(x: Expr[Any], y: Expr[Any], ord: Expr[Int])(using Type[Alts])(using qctx: QuoteContext): Expr[Boolean] =
    '{ ??? : Alts } match {
      case '{ $_ : ($alt *: $alts1) } =>
        '{
          if ($ord == $n)
            ${
              Expr.summon(using '[Mirror.ProductOf[$alt]]) match {
                case Some('{ $mm: $tt }) =>
                  '{
                    val m = $mm
                    type ET = m.ElemTypes
                    ${ eqlElemsExpr[ET](Expr(0))(x, y)(using '[ET]) }
                  }
                case _ =>
                  qctx.throwError(s"Could not find given for ProductOf[${alt.show}]")
              }
            }
          else ${ eqlCasesExpr(Expr(n.unlift.get + 1))(x, y, ord)(using alts1) }
        }
      case '{ $x: Unit } =>
        '{ false }
    }

  inline def derived[T](implicit inline ev: Mirror.Of[T]): Eq[T] =
    ${ derivedExpr[T]('ev) }

  private def derivedExpr[T: Type](using QuoteContext)(ev: Expr[Mirror.Of[T]]): Expr[Eq[T]] = '{
    new Eq[T] {
      def eql(x: T, y: T): Boolean = ${
        ev match
          case '{ $ev: Mirror.SumOf[T] } =>
            '{
              val m = $ev
              val ord = m.ordinal(x)
              type ElemTypes = m.ElemTypes
              ord == m.ordinal(y) && ${ eqlCasesExpr[ElemTypes]('{0})('x, 'y, 'ord)(using '[ElemTypes]) }
            }
          case '{ $ev: Mirror.ProductOf[T] } =>
            '{
              val m = $ev
              type ElemTypes = m.ElemTypes
              ${ eqlElemsExpr[ElemTypes]('{0})('x, 'y)(using '[ElemTypes]) }
            }
      }
    }
  }

  implicit object IntEq extends Eq[Int] {
    def eql(x: Int, y: Int) = x == y
  }
}

// ----------- Another typeclass -----------------------------------

trait Pickler[T] {
  def pickle(buf: mutable.ListBuffer[Int], x: T): Unit
  def unpickle(buf: mutable.ListBuffer[Int]): T
}

object Pickler {
  import scala.compiletime.{erasedValue, constValue, summonFrom}

  def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.trimStart(1)

  inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit =
    ${ tryPickleExpr('buf, 'x) }

  private def tryPickleExpr[T: Type](buf: Expr[mutable.ListBuffer[Int]], x: Expr[T])(using qctx: QuoteContext): Expr[Unit] = {
    Expr.summon[Pickler[T]] match {
      case Some(pkl) => '{ $pkl.pickle($buf, $x) }
      case None =>
        qctx.error(s"Could not find delegate for Pickler[${summon[Type[T]].show}]")
        '{}
    }
  }

  inline def pickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any): Unit =
    ${ pickleElemsExpr[Elems]('n)('buf, 'x) }

  def pickleElemsExpr[Elems <: Tuple : Type](n: Expr[Int])(buf: Expr[mutable.ListBuffer[Int]], x: Expr[Any])(using QuoteContext): Expr[Unit] =
    '{ ??? : Elems } match {
      case '{ type $elems1 <: Tuple; $_ : ($elem *: `$elems1`) } =>
        '{
          ${ tryPickleExpr(buf,  '{ productElement[$elem]($x, $n) })(using elem, summon) }
          ${ pickleElemsExpr('{$n + 1})(buf, x)(using elems1, summon) }
        }
      case '{ $_ : Unit } =>
        '{}
    }

  inline def pickleCases[Alts <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any, ord: Int): Unit =
    inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        if (ord == n)
          summonFrom {
            case m: Mirror.ProductOf[`alt`] => pickleElems[m.ElemTypes](0)(buf, x)
          }
        else pickleCases[alts1](n + 1)(buf, x, ord)
      case _: Unit =>
    }

  inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T =
    ${ tryUnpickleExpr[T]('buf) }

  private def tryUnpickleExpr[T: Type](buf: Expr[mutable.ListBuffer[Int]])(using qctx: QuoteContext): Expr[T] = {
    Expr.summon[Pickler[T]] match {
      case Some(pkl) => '{ $pkl.unpickle($buf) }
      case None =>
        qctx.error(s"Could not find delegate for Pickler[${summon[Type[T]].show}]")
        '{ ??? }
    }
  }

  inline def unpickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], elems: ArrayProduct): Unit =
    ${ unpickleElemsExpr[Elems]('n)('buf, 'elems) }

  private def unpickleElemsExpr[Elems <: Tuple : Type](n: Expr[Int])(buf: Expr[mutable.ListBuffer[Int]], elems: Expr[ArrayProduct])(using QuoteContext): Expr[Unit] =
    '{ ??? : Elems } match {
      case '{ type $elems1 <: Tuple; $_ : ($elem *: `$elems1`) } =>
        '{
          $elems.update($n, ${ tryUnpickleExpr(buf)(using elem, summon) }.asInstanceOf[AnyRef])
          ${ unpickleElemsExpr('{$n + 1})(buf, elems)(using elems1, summon) }
        }
      case '{ $_ : Unit } =>
        '{}
    }

  inline def unpickleCase[T, Elems <: Tuple](buf: mutable.ListBuffer[Int], m: Mirror.ProductOf[T]): T = {
    inline val size = constValue[Tuple.Size[Elems]]
    inline if (size == 0)
      m._fromProduct(EmptyProduct)
    else {
      val elems = new ArrayProduct(size)
      unpickleElems[Elems](0)(buf, elems)
      m._fromProduct(elems)
    }
  }

  inline def unpickleCases[T, Alts <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], ord: Int): T =
    inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        if (ord == n)
          summonFrom {
            case m: Mirror.ProductOf[`alt` & T] =>
              unpickleCase[`alt` & T, m.ElemTypes](buf, m)
          }
        else unpickleCases[T, alts1](n + 1)(buf, ord)
      case _: Unit =>
        throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ord")
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): Pickler[T] = new {
    def pickle(buf: mutable.ListBuffer[Int], x: T): Unit =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          val ord = m.ordinal(x)
          buf += ord
          pickleCases[m.ElemTypes](0)(buf, x, ord)
        case m: Mirror.ProductOf[T] =>
          pickleElems[m.ElemTypes](0)(buf, x)
      }
    def unpickle(buf: mutable.ListBuffer[Int]): T =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          val ord = nextInt(buf)
          unpickleCases[T, m.ElemTypes](0)(buf, ord)
        case m: Mirror.ProductOf[T] =>
          unpickleCase[T, m.ElemTypes](buf, m)
      }
  }

  implicit object IntPickler extends Pickler[Int] {
    def pickle(buf: mutable.ListBuffer[Int], x: Int): Unit = buf += x
    def unpickle(buf: mutable.ListBuffer[Int]): Int = nextInt(buf)
  }
}

// ----------- A third typeclass, making use of labels --------------------------

trait Show[T] {
  def show(x: T): String
}
object Show {
  import scala.compiletime.{erasedValue, constValue, summonFrom}

  inline def tryShow[T](x: T): String =
    ${ tryShowExpr('x) }

  private def tryShowExpr[T: Type](x: Expr[T])(using qctx: QuoteContext): Expr[String] = {
    Expr.summon[Show[T]] match {
      case Some(s) => '{ $s.show($x) }
      case None =>
        qctx.error(s"Could not find delegate for Show[${summon[Type[T]].show}]")
        '{ "" }
    }
  }

  inline def showElems[Elems <: Tuple, Labels <: Tuple](n: Int)(x: Any): List[String] =
    ${ unpickleElemsExpr[Elems, Labels]('n)('x) }

  private def unpickleElemsExpr[Elems <: Tuple : Type, Labels <: Tuple : Type](n: Expr[Int])(x: Expr[Any])(using QuoteContext): Expr[List[String]] =
    '{ ??? : Elems } match {
      case '{ type $elems1 <: Tuple; $_ : ($elem *: `$elems1`) } =>
        '{ ??? : Labels } match {
          case '{ type $labels1 <: Tuple; $_ : ($label *: `$labels1`) } =>
            val Const.Expr(label0) = label
            '{
              val formal = $label0
              val actual = ${ tryShowExpr('{productElement[$elem]($x, $n)}) }
              s"$formal = $actual" :: ${ unpickleElemsExpr('{$n + 1})(x)(using elems1, labels1, summon) }
            }
        }
      case '{ $_ : Unit } =>
        '{ Nil }
    }

  inline def showCase(x: Any, m: Mirror.ProductOf[_]): String = {
    val label = constValue[m.CaseLabel]
    inline m match {
      case m: Mirror.Singleton => label
      case _ => showElems[m.ElemTypes, m.ElemLabels](0)(x).mkString(s"$label(", ", ", ")")
    }
  }

  inline def showCases[Alts <: Tuple](n: Int)(x: Any, ord: Int): String =
    inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        if (ord == n)
          summonFrom {
            case m: Mirror.ProductOf[`alt`] =>
              showCase(x, m)
          }
        else showCases[alts1](n + 1)(x, ord)
      case _: Unit =>
        throw new MatchError(x)
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): Show[T] = new {
    def show(x: T): String =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          val ord = m.ordinal(x)
          showCases[m.ElemTypes](0)(x, ord)
        case m: Mirror.ProductOf[T] =>
          showCase(x, m)
      }
  }

  implicit object IntShow extends Show[Int] {
    def show(x: Int): String = x.toString
  }
}
