
import UnrolledExpr._

import scala.reflect.ClassTag
import scala.quoted._

object Lifters {
  implicit def LiftedClassTag[T: Staged: ClassTag] (using QuoteContext): Expr[ClassTag[T]] = {
    '{ ClassTag(${Expr(summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])}) }
  }

  implicit def ArrayIsLiftable[T : Staged: ClassTag](implicit l: Liftable[T]): Liftable[Array[T]] = new Liftable[Array[T]] {
    def toExpr(x: Array[T]) = '{
      val array = new Array[T](${Expr(x.length)})(${implicitly[Expr[ClassTag[T]]]})
      ${initArray(x, 'array)}
    }
  }

  implicit def IntArrayIsLiftable: Liftable[Array[Int]] = new Liftable[Array[Int]] {
    def toExpr(x: Array[Int]) = '{
      val array = new Array[Int](${Expr(x.length)})
      ${initArray(x, 'array)}
    }
  }

  private def initArray[T : Liftable : Staged](arr: Array[T], array: Expr[Array[T]])(using QuoteContext): Expr[Array[T]] = {
    UnrolledExpr.block(
      arr.zipWithIndex.map {
        case (x, i) => '{ $array(${Expr(i)}) = ${Expr(x)} }
      }.toList,
      array)
  }
}
