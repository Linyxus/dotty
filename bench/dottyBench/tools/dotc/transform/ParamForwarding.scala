package dottyBench.tools
package dotc
package transform

import core._
import ast.Trees._
import Contexts._, Types._, Symbols._, Flags._, TypeUtils._, DenotTransformers._, StdNames._
import Decorators._
import MegaPhase._
import NameKinds.ParamAccessorName
import config.Printers.typr

/** For all private parameter accessors
 *
 *      private val x: T = ...
 *
 *  If there is a chain of parameter accessors starting with `x` such that
 *  (1) The last parameter accessor in the chain is a field that's accessible
 *      from the current class, and
 *  (2) each preceding parameter is forwarded in the supercall of its class
 *      to a parameter that's also named `x`
 *  then change the accessor to
 *
 *      private def x$accessor: T = super.x'.asInstanceOf[T]
 *
 *  where x' is a reference to the final parameter in the chain.
 *  Property (1) is established by the @see forwardParamAccessors method in PostTyper.
 *
 *  The reason for renaming `x` to `x$accessor` is that private methods in the JVM
 *  cannot override public ones.
 *
 *  The aim of this transformation is to avoid redundant parameter accessor fields.
 */
class ParamForwarding extends MiniPhase with IdentityDenotTransformer:
  import ast.tpd._

  private def thisPhase: ParamForwarding = this

  val phaseName: String = "paramForwarding"

  def transformIfParamAlias(mdef: ValOrDefDef)(using Ctx, CState): Tree =

    def inheritedAccessor(sym: Symbol)(using Ctx, CState): Symbol =
      val candidate = sym.owner.asClass.superClass
        .info.decl(sym.name).suchThat(_.is(ParamAccessor, butNot = Mutable))
        .symbol
      if !candidate.is(Private)  // candidate might be private and accessible if it is in an outer class
         && candidate.isAccessibleFrom(currentClass.thisType, superAccess = true)
      then
        candidate
      else if candidate.is(SuperParamAlias) then
        inheritedAccessor(candidate)
      else
        NoSymbol

    val sym = mdef.symbol.asTerm
    if sym.is(SuperParamAlias) then
      assert(sym.is(ParamAccessor, butNot = Mutable))
      val alias = atPhase(thisPhase)(inheritedAccessor(sym))
      if alias.exists then
        sym.copySymDenotation(
            name = ParamAccessorName(sym.name),
            initFlags = sym.flags | Method | StableRealizable,
            info = sym.info.ensureMethodic
          ).installAfter(thisPhase)
        val superAcc =
          Super(This(currentClass), tpnme.EMPTY)
            .withSpan(mdef.span)
            .select(alias)
            .ensureApplied
            .ensureConforms(sym.info.finalResultType)
        report.log(i"adding param forwarder $superAcc")
        DefDef(sym, superAcc)
      else mdef
    else mdef
  end transformIfParamAlias

  override def transformValDef(mdef: ValDef)(using Ctx, CState): Tree =
    transformIfParamAlias(mdef)

  override def transformDefDef(mdef: DefDef)(using Ctx, CState): Tree =
    transformIfParamAlias(mdef)