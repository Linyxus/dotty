package dottyBench.tools.dotc
package transform

import core.Names.Name
import core.DenotTransformers._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Flags._
import core.StdNames._
import core.NameKinds.{DocArtifactName, OuterSelectName}
import core.Decorators._
import core.Phases._
import core.Mode
import typer._
import typer.ErrorReporting._
import reporting._
import ast.Trees._
import ast.{tpd, untpd}
import scala.internal.Chars._
import collection.mutable
import ProtoTypes._

import dottyBench.tools.backend.jvm.DottyBackendInterface.symExtensions

import scala.util.control.NonFatal

/** Run by -Ycheck option after a given phase, this class retypes all syntax trees
 *  and verifies that the type of each tree node so obtained conforms to the type found in the tree node.
 *  It also performs the following checks:
 *
 *   - The owner of each definition is the same as the owner of the current typing context.
 *   - Ident nodes do not refer to a denotation that would need a select to be accessible
 *     (see tpd.needsSelect).
 *   - After typer, identifiers and select nodes refer to terms only (all types should be
 *     represented as TypeTrees then).
 */
class TreeChecker extends Phase with SymTransformer {
  import ast.tpd._
  import TreeChecker._

  private val seenClasses = collection.mutable.HashMap[String, Symbol]()
  private val seenModuleVals = collection.mutable.HashMap[String, Symbol]()

  def isValidJVMName(name: Name): Boolean = name.toString.forall(isValidJVMChar)

  def isValidJVMMethodName(name: Name): Boolean = name.toString.forall(isValidJVMMethodChar)

  val NoSuperClassFlags: FlagSet = Trait | Package

  def testDuplicate(sym: Symbol, registry: mutable.Map[String, Symbol], typ: String)(using Ctx, CState): Unit = {
    val name = sym.javaClassName
    val isDuplicate = this.flatClasses && registry.contains(name)
    assert(!isDuplicate, s"$typ defined twice $sym ${sym.id} ${registry(name).id}")
    registry(name) = sym
  }

  def checkCompanion(symd: SymDenotation)(using Ctx, CState): Unit = {
    val cur = symd.linkedClass
    val prev = atPhase(currentPhase.prev) {
      symd.symbol.linkedClass
    }

    if (prev.exists)
      assert(cur.exists, i"companion disappeared from $symd")
  }

  def transformSym(symd: SymDenotation)(using Ctx, CState): SymDenotation = {
    val sym = symd.symbol

    if (sym.isClass && !sym.isAbsent()) {
      val validSuperclass = sym.isPrimitiveValueClass || defn.syntheticCoreClasses.contains(sym) ||
        (sym eq defn.ObjectClass) || sym.isOneOf(NoSuperClassFlags) || (sym.asClass.superClass.exists) ||
        sym.isRefinementClass

      assert(validSuperclass, i"$sym has no superclass set")
      testDuplicate(sym, seenClasses, "class")
    }

    val badDeferredAndPrivate =
      sym.is(Method) && sym.is(Deferred) && sym.is(Private)
      && !sym.hasAnnotation(defn.NativeAnnot)
      && !sym.isEffectivelyErased

    assert(!badDeferredAndPrivate, i"$sym is both Deferred and Private")

    checkCompanion(symd)

    // Signatures are used to disambiguate overloads and need to stay stable
    // until erasure, see the comment above `Compiler#phases`.
    if (currentPhaseId <= erasurePhase.id) {
      val cur = symd.info
      val initial = symd.initial.info
      val curSig = cur match {
        case cur: SignatureCachingType =>
          // Bypass the signature cache, it might hide a signature change
          cur.computeSignature
        case _ =>
          cur.signature
      }
      assert(curSig == initial.signature,
        i"""Signature of ${sym.showLocated} changed at phase ${ctx.base.squashed(currentPhase.prev)}
           |Initial info: ${initial}
           |Initial sig : ${initial.signature}
           |Current info: ${cur}
           |Current sig : ${curSig}
           |Current cached sig: ${cur.signature}""")
    }

    symd
  }

  def phaseName: String = "Ycheck"

  def run(using Ctx, CState): Unit =
    if (ctx.settings.YtestPickler.value && currentPhase.prev.isInstanceOf[Pickler])
      report.echo("Skipping Ycheck after pickling with -Ytest-pickler, the returned tree contains stale symbols")
    else if (currentPhase.prev.isCheckable)
      check(ctx.base.allPhases.toIndexedSeq, combinedContext)

  private def previousPhases(phases: List[Phase])(using Ctx, CState): List[Phase] = phases match {
    case (phase: MegaPhase) :: phases1 =>
      val subPhases = phase.miniPhases
      val previousSubPhases = previousPhases(subPhases.toList)
      if (previousSubPhases.length == subPhases.length) previousSubPhases ::: previousPhases(phases1)
      else previousSubPhases
    case phase :: phases1 if phase ne currentPhase =>
      phase :: previousPhases(phases1)
    case _ =>
      Nil
  }

  def check(phasesToRun: Seq[Phase], ctx: Context): Tree = {
    val prevPhase = currentPhase(using ctx, ctx.cstate).prev // can be a mini-phase
    val squashedPhase = ctx.base.squashed(prevPhase)
    report.echo(s"checking ${ctx.compilationUnit} after phase ${squashedPhase}")(using ctx, ctx.cstate)

    inContext(ctx) {
      assertSelectWrapsNew(ctx.compilationUnit.tpdTree)
    }

    val checkingCtx = ctx
        .fresh
        .setMode(Mode.ImplicitsEnabled)
        .setReporter(new ThrowingReporter(ctx.reporter))

    val checker = inContext(ctx) {
      new Checker(previousPhases(phasesToRun.toList))
    }
    try checker.typedExpr(ctx.compilationUnit.tpdTree)(using checkingCtx, checkingCtx.cstate)
    catch {
      case NonFatal(ex) =>     //TODO CHECK. Check that we are bootstrapped
        inContext(checkingCtx) {
          println(i"*** error while checking ${ctx.compilationUnit} after phase ${currentPhase.prev} ***")
        }
        throw ex
    }
  }

  class Checker(phasesToCheck: Seq[Phase]) extends ReTyper with Checking {

    private val nowDefinedSyms = new mutable.HashSet[Symbol]
    private val patBoundSyms = new mutable.HashSet[Symbol]
    private val everDefinedSyms = newMutableSymbolMap[untpd.Tree]

    // don't check value classes after typer, as the constraint about constructors doesn't hold after transform
    override def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(using Ctx, CState): Unit = ()

    def withDefinedSyms[T](trees: List[untpd.Tree])(op: => T)(using Ctx, CState): T = {
      var locally = List.empty[Symbol]
      for (tree <- trees) {
        val sym = tree.symbol
        tree match {
          case tree: untpd.DefTree =>
            assert(isValidJVMName(sym.name.encode), s"${sym.name.debugString} name is invalid on jvm")
            everDefinedSyms.get(sym) match {
              case Some(t)  =>
                if (t ne tree)
                  report.warning(i"symbol ${sym.fullName} is defined at least twice in different parts of AST")
              // should become an error
              case None =>
                everDefinedSyms(sym) = tree
            }
            assert(!nowDefinedSyms.contains(sym), i"doubly defined symbol: ${sym.fullName} in $tree")

            if (ctx.settings.YcheckMods.value)
              tree match {
                case t: untpd.MemberDef =>
                  if (t.name ne sym.name) report.warning(s"symbol ${sym.fullName} name doesn't correspond to AST: ${t}")
                // todo: compare trees inside annotations
                case _ =>
              }
            locally = sym :: locally
            nowDefinedSyms += sym
          case _ =>
        }
      }
      val res = op
      nowDefinedSyms --= locally
      res
    }

    /** The following invariant holds:
     *
     *  patBoundSyms.contains(sym) <=> sym.isPatternBound
     */
    def withPatSyms[T](syms: List[Symbol])(op: => T)(using Ctx, CState): T = {
      syms.foreach { sym =>
        assert(
          sym.isPatternBound,
          "patBoundSyms.contains(sym) => sym.isPatternBound is broken." +
          i" Pattern bound symbol $sym has incorrect flags: " + sym.flagsString + ", line " + sym.sourcePos.line
        )
      }
      patBoundSyms ++= syms
      val res = op
      patBoundSyms --= syms
      res
    }

    // used to check invariant of lambda encoding
    var nestingBlock: untpd.Block | Null = null
    private def withBlock[T](block: untpd.Block)(op: => T): T = {
      val outerBlock = nestingBlock
      nestingBlock = block
      val res = op
      nestingBlock = outerBlock
      res
    }

    def assertDefined(tree: untpd.Tree)(using Ctx, CState): Unit =
      if (tree.symbol.maybeOwner.isTerm) {
        val sym = tree.symbol
        assert(
          nowDefinedSyms.contains(sym) || patBoundSyms.contains(sym),
          i"undefined symbol ${sym} at line " + tree.sourcePos.line
        )

        if (!currentPhase.patternTranslated)
          assert(
            !sym.isPatternBound || patBoundSyms.contains(sym),
            i"sym.isPatternBound => patBoundSyms.contains(sym) is broken, sym = $sym, line " + tree.sourcePos.line
          )
      }

    /** assert Java classes are not used as objects */
    def assertIdentNotJavaClass(tree: Tree)(using Ctx, CState): Unit = tree match {
      case _ : untpd.Ident =>
        assert(!tree.symbol.isAllOf(JavaModule), "Java class can't be used as value: " + tree)
      case _ =>
    }

    /** check Java classes are not used as objects */
    def checkIdentNotJavaClass(tree: Tree)(using Ctx, CState): Unit = tree match {
      // case tree: untpd.Ident =>
      // case tree: untpd.Select =>
      // case tree: untpd.Bind =>
      case vd : ValDef =>
        assertIdentNotJavaClass(vd.forceIfLazy)
      case dd : DefDef =>
        assertIdentNotJavaClass(dd.forceIfLazy)
      // case tree: untpd.TypeDef =>
      case Apply(fun, args) =>
        assertIdentNotJavaClass(fun)
        args.foreach(assertIdentNotJavaClass _)
      // case tree: untpd.This =>
      // case tree: untpd.Literal =>
      // case tree: untpd.New =>
      case Typed(expr, _) =>
        assertIdentNotJavaClass(expr)
      case NamedArg(_, arg) =>
        assertIdentNotJavaClass(arg)
      case Assign(_, rhs) =>
        assertIdentNotJavaClass(rhs)
      case Block(stats, expr) =>
        stats.foreach(assertIdentNotJavaClass _)
        assertIdentNotJavaClass(expr)
      case If(_, thenp, elsep) =>
        assertIdentNotJavaClass(thenp)
        assertIdentNotJavaClass(elsep)
      // case tree: untpd.Closure =>
      case Match(selector, cases) =>
        assertIdentNotJavaClass(selector)
        cases.foreach(caseDef => assertIdentNotJavaClass(caseDef.body))
      case Return(expr, _) =>
        assertIdentNotJavaClass(expr)
      case Try(expr, cases, finalizer) =>
        assertIdentNotJavaClass(expr)
        cases.foreach(caseDef => assertIdentNotJavaClass(caseDef.body))
        assertIdentNotJavaClass(finalizer)
      // case tree: TypeApply =>
      // case tree: Super =>
      case SeqLiteral(elems, _) =>
        elems.foreach(assertIdentNotJavaClass)
      // case tree: TypeTree =>
      // case tree: SingletonTypeTree =>
      // case tree: RefinedTypeTree =>
      // case tree: AppliedTypeTree =>
      // case tree: ByNameTypeTree =>
      // case tree: TypeBoundsTree =>
      // case tree: Alternative =>
      // case tree: PackageDef =>
      case Annotated(arg, _) =>
        assertIdentNotJavaClass(arg)
      case _ =>
    }

    /** Exclude from double definition checks any erased symbols that were
     *  made `private` in phase `UnlinkErasedDecls`. These symbols will be removed
     *  completely in phase `Erasure` if they are defined in a currently compiled unit.
     */
    override def excludeFromDoubleDeclCheck(sym: Symbol)(using Ctx, CState): Boolean =
      sym.isEffectivelyErased && sym.is(Private) && !sym.initial.is(Private)

    override def typed(tree: untpd.Tree, pt: Type = WildcardType)(using Ctx, CState): Tree = {
      val tpdTree = super.typed(tree, pt)
      Typer.assertPositioned(tree)
      if (currentlyAfterErasure)
        // Can't be checked in earlier phases since `checkValue` is only run in
        // Erasure (because running it in Typer would force too much)
        checkIdentNotJavaClass(tpdTree)
      tpdTree
    }

    override def typedUnadapted(tree: untpd.Tree, pt: Type, locked: TypeVars)(using Ctx, CState): Tree = {
      val res = tree match {
        case _: untpd.TypedSplice | _: untpd.Thicket | _: EmptyValDef[?] =>
          super.typedUnadapted(tree, pt, locked)
        case _ if tree.isType =>
          promote(tree)
        case _ =>
          val tree1 = super.typedUnadapted(tree, pt, locked)
          def isSubType(tp1: Type, tp2: Type) =
            (tp1 eq tp2) || // accept NoType / NoType
            (tp1 <:< tp2)
          def divergenceMsg(tp1: Type, tp2: Type) =
            s"""Types differ
               |Original type : ${tree.typeOpt.show}
               |After checking: ${tree1.tpe.show}
               |Original tree : ${tree.show}
               |After checking: ${tree1.show}
               |Why different :
             """.stripMargin + core.TypeComparer.explained(tp1 <:< tp2)
          if (tree.hasType) // it might not be typed because Typer sometimes constructs new untyped trees and resubmits them to typedUnadapted
            assert(isSubType(tree1.tpe, tree.typeOpt), divergenceMsg(tree1.tpe, tree.typeOpt))
          tree1
      }
      checkNoOrphans(res.tpe)
      phasesToCheck.foreach(_.checkPostCondition(res))
      res
    }

    def checkNotRepeated(tree: Tree)(using Ctx, CState): tree.type = {
      def allowedRepeated = tree.tpe.widen.isRepeatedParam

      assert(!tree.tpe.widen.isRepeatedParam || allowedRepeated, i"repeated parameter type not allowed here: $tree")
      tree
    }

    /** Check that all methods have MethodicType */
    def isMethodType(pt: Type)(using Ctx, CState): Boolean = pt match {
      case at: AnnotatedType => isMethodType(at.parent)
      case _: MethodicType => true  // MethodType, ExprType, PolyType
      case _ => false
    }

    override def typedIdent(tree: untpd.Ident, pt: Type)(using Ctx, CState): Tree = {
      assert(tree.isTerm || !currentlyAfterTyper, tree.show + " at " + currentPhase)
      assert(tree.isType || ctx.mode.is(Mode.Pattern) && untpd.isWildcardArg(tree) || !needsSelect(tree.tpe), i"bad type ${tree.tpe} for $tree # ${tree.uniqueId}")
      assertDefined(tree)

      checkNotRepeated(super.typedIdent(tree, pt))
    }

    /** Makes sure the symbol in the tree can be approximately reconstructed by
     *  calling `member` on the qualifier type.
     *  Approximately means: The two symbols might be different but one still overrides the other.
     */
    override def typedSelect(tree: untpd.Select, pt: Type)(using Ctx, CState): Tree = {
      assert(tree.isTerm || !currentlyAfterTyper, tree.show + " at " + currentPhase)
      val tpe = tree.typeOpt
      val sym = tree.symbol
      val symIsFixed = tpe match {
        case tpe: TermRef => currentlyAfterErasure || !tpe.isMemberRef
        case _ => false
      }
      if (sym.exists && !sym.is(Private) &&
          !symIsFixed &&
          !tree.name.is(OuterSelectName)) { // outer selects have effectively fixed symbols
        val qualTpe = tree.qualifier.typeOpt
        val member =
          if (sym.is(Private)) qualTpe.member(tree.name)
          else qualTpe.nonPrivateMember(tree.name)
        val memberSyms = member.alternatives.map(_.symbol)
        assert(memberSyms.exists(mbr =>
                 sym == mbr ||
                 sym.overriddenSymbol(mbr.owner.asClass) == mbr ||
                 mbr.overriddenSymbol(sym.owner.asClass) == sym),
               ex"""symbols differ for $tree
                   |was                 : $sym
                   |alternatives by type: $memberSyms%, % of types ${memberSyms.map(_.info)}%, %
                   |qualifier type      : ${tree.qualifier.typeOpt}
                   |tree type           : ${tree.typeOpt} of class ${tree.typeOpt.getClass}""")
      }
      checkNotRepeated(super.typedSelect(tree, pt))
    }

    override def typedThis(tree: untpd.This)(using Ctx, CState): Tree = {
      val res = super.typedThis(tree)
      val cls = res.symbol
      assert(cls.isStaticOwner || ctx.owner.isContainedIn(cls), i"error while typing $tree, ${ctx.owner} is not contained in $cls")
      res
    }

    private def checkOwner(tree: untpd.Tree)(using Ctx, CState): Unit = {
      def ownerMatches(symOwner: Symbol, ctxOwner: Symbol): Boolean =
        symOwner == ctxOwner ||
        ctxOwner.isWeakOwner && ownerMatches(symOwner, ctxOwner.owner)
      assert(ownerMatches(tree.symbol.owner, ctx.owner),
        i"bad owner; ${tree.symbol} has owner ${tree.symbol.owner}, expected was ${ctx.owner}\n" +
        i"owner chain = ${tree.symbol.ownersIterator.toList}%, %, ctxOwners = ${ctx.outersIterator.map(_.owner).toList}%, %")
    }

    override def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(using Ctx, CState): Tree = {
      val TypeDef(_, impl @ Template(constr, _, _, _)) = cdef
      assert(cdef.symbol == cls)
      assert(impl.symbol.owner == cls)
      assert(constr.symbol.owner == cls)
      assert(cls.primaryConstructor == constr.symbol, i"mismatch, primary constructor ${cls.primaryConstructor}, in tree = ${constr.symbol}")
      checkOwner(impl)
      checkOwner(impl.constr)

      def isNonMagicalMember(x: Symbol) =
        !x.isValueClassConvertMethod &&
        !x.name.is(DocArtifactName)

      val decls   = cls.classInfo.decls.toList.toSet.filter(isNonMagicalMember)
      val defined = impl.body.map(_.symbol)

      val symbolsNotDefined = decls -- defined - constr.symbol

      assert(symbolsNotDefined.isEmpty,
          i" $cls tree does not define members: ${symbolsNotDefined.toList}%, %\n" +
          i"expected: ${decls.toList}%, %\n" +
          i"defined: ${defined}%, %")

      super.typedClassDef(cdef, cls)
    }

    override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(using Ctx, CState): Tree =
      def defParamss =
        (ddef.tparams :: ddef.vparamss).filter(!_.isEmpty).map(_.map(_.symbol))
      def layout(symss: List[List[Symbol]]): String =
        symss.map(syms => i"($syms%, %)").mkString
      assert(currentlyAfterErasure || sym.rawParamss == defParamss,
        i"""param mismatch for ${sym.showLocated}:
           |defined in tree  = ${layout(defParamss)}
           |stored in symbol = ${layout(sym.rawParamss)}""")
      withDefinedSyms(ddef.tparams) {
        withDefinedSyms(ddef.vparamss.flatten) {
          if (!sym.isClassConstructor && !(sym.name eq nme.STATIC_CONSTRUCTOR))
            assert(isValidJVMMethodName(sym.name.encode), s"${sym.name.debugString} name is invalid on jvm")

          ddef.vparamss.foreach(_.foreach { vparam =>
            assert(vparam.symbol.is(Param),
              s"Parameter ${vparam.symbol} of ${sym.fullName} does not have flag `Param` set")
            assert(!vparam.symbol.isOneOf(AccessFlags),
              s"Parameter ${vparam.symbol} of ${sym.fullName} has invalid flag(s): ${(vparam.symbol.flags & AccessFlags).flagsString}")
          })

          val tpdTree = super.typedDefDef(ddef, sym)
          assert(isMethodType(sym.info), i"wrong type, expect a method type for ${sym.fullName}, but found: ${sym.info}")
          tpdTree
        }
      }

    override def typedCase(tree: untpd.CaseDef, sel: Tree, selType: Type, pt: Type)(using Ctx, CState): CaseDef =
      withPatSyms(tpd.patVars(tree.pat.asInstanceOf[tpd.Tree])) {
        super.typedCase(tree, sel, selType, pt)
      }

    override def typedClosure(tree: untpd.Closure, pt: Type)(using Ctx, CState): Tree = {
      if (!currentPhase.lambdaLifted) nestingBlock match {
        case block @ Block((meth : untpd.DefDef) :: Nil, closure: untpd.Closure) =>
          assert(meth.symbol == closure.meth.symbol, "closure.meth symbol not equal to method symbol. Block: " + block.show)

        case block: untpd.Block =>
          assert(false, "function literal are not properly formed as a block of DefDef and Closure. Found: " + tree.show + " Nesting block: " + block.show)

        case null =>
          assert(false, "function literal are not properly formed as a block of DefDef and Closure. Found: " + tree.show + " Nesting block: null")
      }
      super.typedClosure(tree, pt)
    }

    override def typedBlock(tree: untpd.Block, pt: Type)(using Ctx, CState): Tree =
      withBlock(tree) { withDefinedSyms(tree.stats) { super.typedBlock(tree, pt) } }

    override def typedInlined(tree: untpd.Inlined, pt: Type)(using Ctx, CState): Tree =
      withDefinedSyms(tree.bindings) { super.typedInlined(tree, pt) }

    /** Check that all defined symbols have legal owners.
     *  An owner is legal if it is either the same as the context's owner
     *  or there's an owner chain of valdefs starting at the context's owner and
     *  reaching up to the symbol's owner. The reason for this relaxed matching
     *  is that we should be able to pull out an expression as an initializer
     *  of a helper value without having to do a change owner traversal of the expression.
     */
    override def typedStats(trees: List[untpd.Tree], exprOwner: Symbol)(using Ctx, CState): (List[Tree], Ctx) = {
      for (tree <- trees) tree match {
        case tree: untpd.DefTree => checkOwner(tree)
        case _: untpd.Thicket => assert(false, i"unexpanded thicket $tree in statement sequence $trees%\n%")
        case _ =>
      }
      super.typedStats(trees, exprOwner)
    }

    override def typedLabeled(tree: untpd.Labeled)(using Ctx, CState): Labeled = {
      checkOwner(tree.bind)
      withDefinedSyms(tree.bind :: Nil) { super.typedLabeled(tree) }
    }

    override def typedReturn(tree: untpd.Return)(using Ctx, CState): Return = {
      val tree1 = super.typedReturn(tree)
      val from = tree1.from
      val fromSym = from.symbol
      if (fromSym.is(Label))
        assertDefined(from)
      tree1
    }

    override def typedWhileDo(tree: untpd.WhileDo)(using Ctx, CState): Tree = {
      assert((tree.cond ne EmptyTree) || currentPhase.refChecked, i"invalid empty condition in while at $tree")
      super.typedWhileDo(tree)
    }

    override def ensureNoLocalRefs(tree: Tree, pt: Type, localSyms: => List[Symbol])(using Ctx, CState): Tree =
      tree

    override def adapt(tree: Tree, pt: Type, locked: TypeVars, tryGadtHealing: Boolean)(using Ctx, CState): Tree = {
      def isPrimaryConstructorReturn =
        ctx.owner.isPrimaryConstructor && pt.isRef(ctx.owner.owner) && tree.tpe.isRef(defn.UnitClass)
      def infoStr(tp: Type) = tp match {
        case tp: TypeRef =>
          val sym = tp.symbol
          i"${sym.showLocated} with ${tp.designator}, flags = ${sym.flagsString}, underlying = ${tp.underlyingIterator.toList}%, %"
        case _ =>
          "??"
      }
      if (ctx.mode.isExpr &&
          !tree.isEmpty &&
          !isPrimaryConstructorReturn &&
          !pt.isInstanceOf[FunOrPolyProto])
        assert(tree.tpe <:< pt, {
          val mismatch = TypeMismatch(tree.tpe, pt)
          i"""|${mismatch.msg}
              |found: ${infoStr(tree.tpe)}
              |expected: ${infoStr(pt)}
              |tree = $tree""".stripMargin
        })
      tree
    }

    override def simplify(tree: Tree, pt: Type, locked: TypeVars)(using Ctx, CState): tree.type = tree
  }

  /**
    * Checks that `New` nodes are always wrapped inside `Select` nodes.
    */
  def assertSelectWrapsNew(tree: Tree)(using Ctx, CState): Unit =
    (new TreeAccumulator[tpd.Tree] {
      override def apply(parent: Tree, tree: Tree)(using Ctx, CState): Tree = {
        tree match {
          case tree: New if !parent.isInstanceOf[tpd.Select] =>
            assert(assertion = false, i"`New` node must be wrapped in a `Select`:\n  parent = ${parent.show}\n  child = ${tree.show}")
          case _: Annotated =>
            // Don't check inside annotations, since they're allowed to contain
            // somewhat invalid trees.
          case _ =>
            foldOver(tree, tree) // replace the parent when folding over the children
        }
        parent // return the old parent so that my siblings see it
      }
    })(tpd.EmptyTree, tree)
}

object TreeChecker {
  /** - Check that TypeParamRefs and MethodParams refer to an enclosing type.
   *  - Check that all type variables are instantiated.
   */
  def checkNoOrphans(tp0: Type, tree: untpd.Tree = untpd.EmptyTree)(using Ctx, CState): Type = new TypeMap() {
    val definedBinders = new java.util.IdentityHashMap[Type, Any]
    def apply(tp: Type): Type = {
      tp match {
        case tp: BindingType =>
          definedBinders.put(tp, tp)
          mapOver(tp)
          definedBinders.remove(tp)
        case tp: ParamRef =>
          assert(definedBinders.get(tp.binder) != null, s"orphan param: ${tp.show}, hash of binder = ${System.identityHashCode(tp.binder)}, tree = ${tree.show}, type = $tp0")
        case tp: TypeVar =>
          assert(tp.isInstantiated, s"Uninstantiated type variable: ${tp.show}, tree = ${tree.show}")
          apply(tp.underlying)
        case _ =>
          mapOver(tp)
      }
      tp
    }
  }.apply(tp0)
}