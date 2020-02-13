import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving._
import scala.quoted._

trait Lft[T]:
  def toExpr(x: T): Quotes ?=> Expr[T]

object Lft {
  given Lft[Int] with
    def toExpr(x: Int) = Expr(x)

  inline given derived[T](using inline m: Mirror.Of[T]): Lft[T] = ${ derivedExpr('m) }

  private def derivedExpr[T](mirrorExpr: Expr[Mirror.Of[T]])(using qctx: Quotes, tpe: Type[T]): Expr[Lft[T]] = {
    mirrorExpr match {
      case '{ $mirrorExpr : Mirror.Sum { type MirroredElemTypes = mirroredElemTypes } } =>
        val liftableExprs = elemTypesLfts[mirroredElemTypes]
        '{
          new Lft[T]:
            def toExpr(x: T) =
              val mirror = $mirrorExpr
              val liftable = ${Expr.ofSeq(liftableExprs)}.apply(mirror.ordinal(x)).asInstanceOf[Lft[T]] // TODO generate switch
              liftable.toExpr(x)
        }

      case '{ $mirrorExpr : Mirror.Product { type MirroredElemTypes = mirroredElemTypes } } =>
        val liftableExprs = Expr.ofList(elemTypesLfts[mirroredElemTypes])
        '{
          new Lft[T]:
            def toExpr(x: T) =
              val liftables = $liftableExprs
              val lifted = Expr.ofSeq(liftables.zipWithIndex.map { (liftable, i) =>
                liftable.asInstanceOf[Lft[AnyRef]].toExpr(???) // (productElement(x, i))
              })
              val liftedProduct = '{ Tuple.fromArray(Array($lifted: _*)) }
              val mirror = Expr.summon(using '[Mirror.ProductOf[T]]).get
              '{ $mirror.fromProduct($liftedProduct) }
        }
    }
  }

  private def elemTypesLfts[X: Type](using Quotes): List[Expr[Lft[_]]] =
    Type.of[X] match
      case '[ head *: tail ] =>
        Expr.summon(using '[Lft[head]]).getOrElse(quotes.reflect.report.throwError(s"Could not find given Lft[${Type.show[head]}]")) :: elemTypesLfts[tail]
      case '[ Unit ] => Nil

}
