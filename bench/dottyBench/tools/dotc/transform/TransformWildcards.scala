package dottyBench.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers._
import core.Contexts._
import ast.tpd

/** This phase transforms wildcards in valdefs with their default value.
  *  In particular for every valdef that is declared:
  *    `val x : T = _` to `val x : T = <zero of T>`
  *
  */
class TransformWildcards extends MiniPhase with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = "transformWildcards"

  override def checkPostCondition(tree: Tree)(using Ctx, CState): Unit =
    tree match {
      case vDef: ValDef => assert(!tpd.isWildcardArg(vDef.rhs))
      case _ =>
    }

  override def transformValDef(tree: ValDef)(using Ctx, CState): Tree =
    if (ctx.owner.isClass) tree
    else cpy.ValDef(tree)(rhs = tree.rhs.wildcardToDefault)
}