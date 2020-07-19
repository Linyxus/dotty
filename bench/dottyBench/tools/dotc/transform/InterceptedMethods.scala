package dottyBench.tools.dotc
package transform

import dottyBench.tools.dotc.ast.Trees._
import dottyBench.tools.dotc.ast.tpd
import dottyBench.tools.dotc.core.Constants.Constant
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.core.Names.TermName
import dottyBench.tools.dotc.core.StdNames._
import dottyBench.tools.dotc.core.Symbols._
import dottyBench.tools.dotc.core.Types._
import dottyBench.tools.dotc.transform.MegaPhase.MiniPhase

object InterceptedMethods {
  val name: String = "intercepted"
}

/** Replace member references as follows:
  *
  * - `x != y` for != in class Any becomes `!(x == y)` with == in class Any.
  * - `x.##` for ## in NullClass becomes `0`
  * - `x.##` for ## in Any becomes calls to ScalaRunTime.hash,
  *     using the most precise overload available
  */
class InterceptedMethods extends MiniPhase {
  import tpd._

  override def phaseName: String = InterceptedMethods.name

  // this should be removed if we have guarantee that ## will get Apply node
  override def transformSelect(tree: tpd.Select)(using Ctx, CState): Tree =
    transformRefTree(tree)

  override def transformIdent(tree: tpd.Ident)(using Ctx, CState): Tree =
    transformRefTree(tree)

  private def transformRefTree(tree: RefTree)(using Ctx, CState): Tree =
    if (tree.symbol.isTerm && (defn.Any_## eq tree.symbol)) {
      val qual = tree match {
        case id: Ident => tpd.desugarIdentPrefix(id)
        case sel: Select => sel.qualifier
      }
      val rewritten = poundPoundValue(qual)
      report.log(s"$phaseName rewrote $tree to $rewritten")
      rewritten
    }
    else tree

  // TODO: add missing cases from scalac
  private def poundPoundValue(tree: Tree)(using Ctx, CState) = {
    val s = tree.tpe.widen.typeSymbol

    def staticsCall(methodName: TermName): Tree =
      ref(defn.staticsMethodRef(methodName)).appliedTo(tree)

    if (s == defn.NullClass) Literal(Constant(0))
    else if (s == defn.DoubleClass) staticsCall(nme.doubleHash)
    else if (s == defn.LongClass) staticsCall(nme.longHash)
    else if (s == defn.FloatClass) staticsCall(nme.floatHash)
    else staticsCall(nme.anyHash)
  }

  override def transformApply(tree: Apply)(using Ctx, CState): Tree = {
    lazy val qual = tree.fun match {
      case Select(qual, _) => qual
      case ident @ Ident(_) =>
        ident.tpe match {
          case TermRef(prefix: TermRef, _) =>
            tpd.ref(prefix)
          case TermRef(prefix: ThisType, _) =>
            tpd.This(prefix.cls)
        }
    }

    if tree.fun.symbol == defn.Any_!= then
      qual.select(defn.Any_==).appliedToArgs(tree.args).select(defn.Boolean_!).withSpan(tree.span)
    else
      tree
  }
}