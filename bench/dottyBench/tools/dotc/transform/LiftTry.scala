package dottyBench.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.NameKinds.LiftedTreeName
import NonLocalReturns._
import util.Store

/** Lifts try's that might be executed on non-empty expression stacks
 *  to their own methods. I.e.
 *
 *      try body catch handler
 *
 *  is lifted to
 *
 *      { def liftedTree$n() = try body catch handler; liftedTree$n() }
 *
 *  However, don't lift try's without catch expressions (try-finally).
 *  Lifting is needed only for try-catch expressions that are evaluated in a context
 *  where the stack might not be empty. `finally` does not attempt to continue evaluation
 *  after an exception, so the fact that values on the stack are 'lost' does not matter
 *  (copied from https://github.com/scala/scala/pull/922).
 */
class LiftTry extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  val phaseName: String = LiftTry.name

  private var NeedLift: Store.Location[Boolean] = _
  private def needLift(using Ctx, CState): Boolean = ctx.store(NeedLift)

  override def initContext(ctx: FreshCtx): Unit =
    NeedLift = ctx.addLocation(false)

  private def liftingCtx(p: Boolean)(using Ctx, CState) =
    if (needLift == p) ctx else ctx.fresh.updateStore(NeedLift, p)

  override def prepareForApply(tree: Apply)(using Ctx, CState): Ctx =
    liftingCtx(true)

  override def prepareForValDef(tree: ValDef)(using Ctx, CState): Ctx =
    if !tree.symbol.exists
       || tree.symbol.isSelfSym
       || tree.symbol.owner == ctx.owner.enclosingMethod
          && !tree.symbol.is(Lazy)
            // The current implementation wraps initializers of lazy vals in
            // calls to an initialize method, which means that a `try` in the
            // initializer needs to be lifted. Note that the new scheme proposed
            // in #6979 would avoid this.
    then ctx
    else liftingCtx(true)

  override def prepareForAssign(tree: Assign)(using Ctx, CState): Ctx =
    if (tree.lhs.symbol.maybeOwner == ctx.owner.enclosingMethod) ctx
    else liftingCtx(true)

  override def prepareForReturn(tree: Return)(using Ctx, CState): Ctx =
    if (!isNonLocalReturn(tree)) ctx
    else liftingCtx(true)

  override def prepareForTemplate(tree: Template)(using Ctx, CState): Ctx =
    liftingCtx(false)

  override def transformTry(tree: Try)(using Ctx, CState): Tree =
    if (needLift && tree.cases.nonEmpty) {
      report.debuglog(i"lifting tree at ${tree.span}, current owner = ${ctx.owner}")
      val fn = newSymbol(
        ctx.owner, LiftedTreeName.fresh(), Synthetic | Method,
        MethodType(Nil, tree.tpe.widenIfUnstable), coord = tree.span)
      tree.changeOwnerAfter(ctx.owner, fn, thisPhase)
      Block(DefDef(fn, tree) :: Nil, ref(fn).appliedToNone)
    }
    else tree
}
object LiftTry:
  val name = "liftTry"