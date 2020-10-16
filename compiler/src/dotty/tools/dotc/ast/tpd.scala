package dotty.tools
package dotc
package ast

import dotty.tools.dotc.transform.{ExplicitOuter, Erasure}
import typer.ProtoTypes
import transform.SymUtils._
import transform.TypeUtils._
import core._
import util.Spans._, Types._, Contexts._, Constants._, Names._, Flags._, NameOps._
import Symbols._, StdNames._, Annotations._, Trees._, Symbols._
import Decorators._, DenotTransformers._
import collection.{immutable, mutable}
import util.{Property, SourceFile, NoSource}
import util.Lst.{NIL, +:, toLst}
import NameKinds.{TempResultName, OuterSelectName}
import typer.ConstFold
import util.Lst

import scala.annotation.tailrec
import scala.io.Codec

/** Some creators for typed trees */
object tpd extends Trees.Instance[Type] with TypedTreeInfo {

  private def ta(using Context) = ctx.typeAssigner

  def Ident(tp: NamedType)(using Context): Ident =
    ta.assignType(untpd.Ident(tp.name), tp)

  def Select(qualifier: Tree, name: Name)(using Context): Select =
    ta.assignType(untpd.Select(qualifier, name), qualifier)

  def Select(qualifier: Tree, tp: NamedType)(using Context): Select =
    untpd.Select(qualifier, tp.name).withType(tp)

  def This(cls: ClassSymbol)(using Context): This =
    untpd.This(untpd.Ident(cls.name)).withType(cls.thisType)

  def Super(qual: Tree, mix: untpd.Ident, mixinClass: Symbol)(using Context): Super =
    ta.assignType(untpd.Super(qual, mix), qual, mixinClass)

  def Super(qual: Tree, mixName: TypeName, mixinClass: Symbol = NoSymbol)(using Context): Super =
    Super(qual, if (mixName.isEmpty) untpd.EmptyTypeIdent else untpd.Ident(mixName), mixinClass)

  def Apply(fn: Tree, args: Lst[Tree])(using Context): Apply = {
    assert(fn.isInstanceOf[RefTree] || fn.isInstanceOf[GenericApply[_]] || fn.isInstanceOf[Inlined] || fn.isInstanceOf[tasty.TreePickler.Hole])
    ta.assignType(untpd.Apply(fn, args), fn, args)
  }

  def TypeApply(fn: Tree, args: Lst[Tree])(using Context): TypeApply = {
    assert(fn.isInstanceOf[RefTree] || fn.isInstanceOf[GenericApply[_]])
    ta.assignType(untpd.TypeApply(fn, args), fn, args)
  }

  def Literal(const: Constant)(using Context): Literal =
    ta.assignType(untpd.Literal(const))

  def unitLiteral(using Context): Literal =
    Literal(Constant(()))

  def nullLiteral(using Context): Literal =
    Literal(Constant(null))

  def New(tpt: Tree)(using Context): New =
    ta.assignType(untpd.New(tpt), tpt)

  def New(tp: Type)(using Context): New = New(TypeTree(tp))

  def Typed(expr: Tree, tpt: Tree)(using Context): Typed =
    ta.assignType(untpd.Typed(expr, tpt), tpt)

  def NamedArg(name: Name, arg: Tree)(using Context): NamedArg =
    ta.assignType(untpd.NamedArg(name, arg), arg)

  def Assign(lhs: Tree, rhs: Tree)(using Context): Assign =
    ta.assignType(untpd.Assign(lhs, rhs))

  def Block(stats: Lst[Tree], expr: Tree)(using Context): Block =
    ta.assignType(untpd.Block(stats, expr), stats, expr)

  /** Join `stats` in front of `expr` creating a new block if necessary */
  def seq(stats: Lst[Tree], expr: Tree)(using Context): Tree =
    if (stats.isEmpty) expr
    else expr match {
      case Block(_, _: Closure) =>
        Block(stats, expr)  // leave closures in their own block
      case Block(estats, eexpr) =>
        cpy.Block(expr)(stats ::: estats, eexpr).withType(ta.avoidingType(eexpr, stats))
      case _ =>
        Block(stats, expr)
    }

  def If(cond: Tree, thenp: Tree, elsep: Tree)(using Context): If =
    ta.assignType(untpd.If(cond, thenp, elsep), thenp, elsep)

  def InlineIf(cond: Tree, thenp: Tree, elsep: Tree)(using Context): If =
    ta.assignType(untpd.InlineIf(cond, thenp, elsep), thenp, elsep)

  def Closure(env: Lst[Tree], meth: Tree, tpt: Tree)(using Context): Closure =
    ta.assignType(untpd.Closure(env, meth, tpt), meth, tpt)

  /** A function def
   *
   *    vparams => expr
   *
   *  gets expanded to
   *
   *    { def $anonfun(vparams) = expr; Closure($anonfun) }
   *
   *  where the closure's type is the target type of the expression (FunctionN, unless
   *  otherwise specified).
   */
  def Closure(meth: TermSymbol, rhsFn: List[List[Tree]] => Tree, targs: Lst[Tree] = NIL, targetType: Type = NoType)(using Context): Block = {
    val targetTpt = if (targetType.exists) TypeTree(targetType) else EmptyTree
    val call =
      if (targs.isEmpty) Ident(TermRef(NoPrefix, meth))
      else TypeApply(Ident(TermRef(NoPrefix, meth)), targs)
    Block(
      Lst(DefDef(meth, rhsFn)),
      Closure(NIL, call, targetTpt))
  }

  /** A closure whole anonymous function has the given method type */
  def Lambda(tpe: MethodType, rhsFn: List[Tree] => Tree)(using Context): Block = {
    val meth = newSymbol(ctx.owner, nme.ANON_FUN, Synthetic | Method, tpe)
    Closure(meth, tss => rhsFn(tss.head).changeOwner(ctx.owner, meth))
  }

  def CaseDef(pat: Tree, guard: Tree, body: Tree)(using Context): CaseDef =
    ta.assignType(untpd.CaseDef(pat, guard, body), pat, body)

  def Match(selector: Tree, cases: Lst[CaseDef])(using Context): Match =
    ta.assignType(untpd.Match(selector, cases), selector, cases)

  def InlineMatch(selector: Tree, cases: Lst[CaseDef])(using Context): Match =
    ta.assignType(untpd.InlineMatch(selector, cases), selector, cases)

  def Labeled(bind: Bind, expr: Tree)(using Context): Labeled =
    ta.assignType(untpd.Labeled(bind, expr))

  def Labeled(sym: TermSymbol, expr: Tree)(using Context): Labeled =
    Labeled(Bind(sym, EmptyTree), expr)

  def Return(expr: Tree, from: Tree)(using Context): Return =
    ta.assignType(untpd.Return(expr, from))

  def Return(expr: Tree, from: Symbol)(using Context): Return =
    Return(expr, Ident(from.termRef))

  def WhileDo(cond: Tree, body: Tree)(using Context): WhileDo =
    ta.assignType(untpd.WhileDo(cond, body))

  def Try(block: Tree, cases: Lst[CaseDef], finalizer: Tree)(using Context): Try =
    ta.assignType(untpd.Try(block, cases, finalizer), block, cases)

  def SeqLiteral(elems: Lst[Tree], elemtpt: Tree)(using Context): SeqLiteral =
    ta.assignType(untpd.SeqLiteral(elems, elemtpt), elems, elemtpt)

  def JavaSeqLiteral(elems: Lst[Tree], elemtpt: Tree)(using Context): JavaSeqLiteral =
    ta.assignType(untpd.JavaSeqLiteral(elems, elemtpt), elems, elemtpt).asInstanceOf[JavaSeqLiteral]

  def Inlined(call: Tree, bindings: Lst[MemberDef], expansion: Tree)(using Context): Inlined =
    ta.assignType(untpd.Inlined(call, bindings, expansion), bindings, expansion)

  def TypeTree(tp: Type)(using Context): TypeTree =
    untpd.TypeTree().withType(tp)

  def SingletonTypeTree(ref: Tree)(using Context): SingletonTypeTree =
    ta.assignType(untpd.SingletonTypeTree(ref), ref)

  def RefinedTypeTree(parent: Tree, refinements: Lst[Tree], refineCls: ClassSymbol)(using Context): Tree =
    ta.assignType(untpd.RefinedTypeTree(parent, refinements), parent, refinements, refineCls)

  def AppliedTypeTree(tycon: Tree, args: Lst[Tree])(using Context): AppliedTypeTree =
    ta.assignType(untpd.AppliedTypeTree(tycon, args), tycon, args)

  def ByNameTypeTree(result: Tree)(using Context): ByNameTypeTree =
    ta.assignType(untpd.ByNameTypeTree(result), result)

  def LambdaTypeTree(tparams: List[TypeDef], body: Tree)(using Context): LambdaTypeTree =
    ta.assignType(untpd.LambdaTypeTree(tparams, body), tparams, body)

  def MatchTypeTree(bound: Tree, selector: Tree, cases: Lst[CaseDef])(using Context): MatchTypeTree =
    ta.assignType(untpd.MatchTypeTree(bound, selector, cases), bound, selector, cases)

  def TypeBoundsTree(lo: Tree, hi: Tree, alias: Tree = EmptyTree)(using Context): TypeBoundsTree =
    ta.assignType(untpd.TypeBoundsTree(lo, hi, alias), lo, hi, alias)

  def Bind(sym: Symbol, body: Tree)(using Context): Bind =
    ta.assignType(untpd.Bind(sym.name, body), sym)

  /** A pattern corresponding to `sym: tpe` */
  def BindTyped(sym: TermSymbol, tpe: Type)(using Context): Bind =
    Bind(sym, Typed(Underscore(tpe), TypeTree(tpe)))

  def Alternative(trees: Lst[Tree])(using Context): Alternative =
    ta.assignType(untpd.Alternative(trees), trees)

  def UnApply(fun: Tree, implicits: Lst[Tree], patterns: Lst[Tree], proto: Type)(using Context): UnApply = {
    assert(fun.isInstanceOf[RefTree] || fun.isInstanceOf[GenericApply[_]])
    ta.assignType(untpd.UnApply(fun, implicits, patterns), proto)
  }

  def ValDef(sym: TermSymbol, rhs: LazyTree = EmptyTree)(using Context): ValDef =
    ta.assignType(untpd.ValDef(sym.name, TypeTree(sym.info), rhs), sym)

  def SyntheticValDef(name: TermName, rhs: Tree)(using Context): ValDef =
    ValDef(newSymbol(ctx.owner, name, Synthetic, rhs.tpe.widen, coord = rhs.span), rhs)

  def DefDef(sym: TermSymbol, tparams: List[TypeSymbol], vparamss: List[List[TermSymbol]],
             resultType: Type, rhs: Tree)(using Context): DefDef =
    sym.setParamss(tparams, vparamss)
    ta.assignType(
      untpd.DefDef(
        sym.name,
        tparams.map(tparam => TypeDef(tparam).withSpan(tparam.span)),
        vparamss.nestedMap(vparam => ValDef(vparam).withSpan(vparam.span)),
        TypeTree(resultType),
        rhs),
      sym)

  def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree)(using Context): DefDef =
    ta.assignType(DefDef(sym, Function.const(rhs) _), sym)

  def DefDef(sym: TermSymbol, rhsFn: List[List[Tree]] => Tree)(using Context): DefDef =
    polyDefDef(sym, Function.const(rhsFn))

  /** A DefDef with given method symbol `sym`.
   *  @rhsFn  A function from type parameter types and term parameter references
   *          to the method's right-hand side.
   *  Parameter symbols are taken from the `rawParamss` field of `sym`, or
   *  are freshly generated if `rawParamss` is empty.
   */
  def polyDefDef(sym: TermSymbol, rhsFn: List[Type] => List[List[Tree]] => Tree)(using Context): DefDef = {

    val (tparams, existingParamss, mtp) = sym.info match {
      case tp: PolyType =>
        val (tparams, existingParamss) = sym.rawParamss match
          case tparams :: vparamss =>
            assert(tparams.hasSameLengthAs(tp.paramNames) && tparams.head.isType)
            (tparams.asInstanceOf[List[TypeSymbol]], vparamss)
          case _ =>
            (newTypeParams(sym, tp.paramNames, EmptyFlags, tp.instantiateParamInfos(_)), Nil)
        (tparams, existingParamss, tp.instantiate(tparams map (_.typeRef)))
      case tp => (Nil, sym.rawParamss, tp)
    }

    def valueParamss(tp: Type, existingParamss: List[List[Symbol]]): (List[List[TermSymbol]], Type) = tp match {
      case tp: MethodType =>
        val isParamDependent = tp.isParamDependent
        val previousParamRefs = if (isParamDependent) mutable.ListBuffer[TermRef]() else null

        def valueParam(name: TermName, origInfo: Type): TermSymbol = {
          val maybeImplicit =
            if (tp.isContextualMethod) Given
            else if (tp.isImplicitMethod) Implicit
            else EmptyFlags
          val maybeErased = if (tp.isErasedMethod) Erased else EmptyFlags

          def makeSym(info: Type) = newSymbol(sym, name, TermParam | maybeImplicit | maybeErased, info, coord = sym.coord)

          if (isParamDependent) {
            val sym = makeSym(origInfo.substParams(tp, previousParamRefs.toList))
            previousParamRefs += sym.termRef
            sym
          }
          else
            makeSym(origInfo)
        }

        val (params, existingParamss1) =
          if tp.paramInfos.isEmpty then (Nil, existingParamss)
          else existingParamss match
            case vparams :: existingParamss1 =>
              assert(vparams.hasSameLengthAs(tp.paramNames) && vparams.head.isTerm)
              (vparams.asInstanceOf[List[TermSymbol]], existingParamss1)
            case _ =>
              (tp.paramNames.lazyZip(tp.paramInfos).map(valueParam), Nil)
        val (paramss, rtp) =
          valueParamss(tp.instantiate(params map (_.termRef)), existingParamss1)
        (params :: paramss, rtp)
      case tp => (Nil, tp.widenExpr)
    }
    val (vparamss, rtp) = valueParamss(mtp, existingParamss)
    val targs = tparams map (_.typeRef)
    val argss = vparamss.nestedMap(vparam => Ident(vparam.termRef))
    sym.setParamss(tparams, vparamss)
    DefDef(sym, tparams, vparamss, rtp, rhsFn(targs)(argss))
  }

  def TypeDef(sym: TypeSymbol)(using Context): TypeDef =
    ta.assignType(untpd.TypeDef(sym.name, TypeTree(sym.info)), sym)

  def ClassDef(cls: ClassSymbol, constr: DefDef, body: Lst[Tree], superArgs: List[Tree] = Nil)(using Context): TypeDef = {
    val firstParent :: otherParents = cls.info.parents
    val superRef =
      if (cls.is(Trait)) TypeTree(firstParent)
      else {
        def isApplicable(ctpe: Type): Boolean = ctpe match {
          case ctpe: PolyType =>
            isApplicable(ctpe.instantiate(firstParent.argTypes))
          case ctpe: MethodType =>
            (superArgs corresponds ctpe.paramInfos)(_.tpe <:< _)
          case _ =>
            false
        }
        val constr = firstParent.decl(nme.CONSTRUCTOR).suchThat(constr => isApplicable(constr.info))
        New(firstParent, constr.symbol.asTerm, superArgs.toLst)
      }
    ClassDefWithParents(cls, constr, superRef :: otherParents.map(TypeTree(_)), body)
  }

  def ClassDefWithParents(cls: ClassSymbol, constr: DefDef, parents: List[Tree], body: Lst[Tree])(using Context): TypeDef = {
    val selfType =
      if (cls.classInfo.selfInfo ne NoType) ValDef(newSelfSym(cls))
      else EmptyValDef
    def isOwnTypeParam(stat: Tree) =
      stat.symbol.is(TypeParam) && stat.symbol.owner == cls
    val bodyTypeParams = body filter isOwnTypeParam map (_.symbol)
    val newTypeParams =
      for (tparam <- cls.typeParams if !(bodyTypeParams contains tparam))
      yield TypeDef(tparam)
    val findLocalDummy = FindLocalDummyAccumulator(cls)
    val localDummy = body.foldLeft(NoSymbol: Symbol)(findLocalDummy.apply)
      .orElse(newLocalDummy(cls))
    val impl = untpd.Template(constr, parents, Nil, selfType, newTypeParams ::: body)
      .withType(localDummy.termRef)
    ta.assignType(untpd.TypeDef(cls.name, impl), cls)
  }

  /** An anonymous class
   *
   *      new parents { forwarders }
   *
   *  where `forwarders` contains forwarders for all functions in `fns`.
   *  @param parents    a non-empty list of class types
   *  @param fns        a non-empty of functions for which forwarders should be defined in the class.
   *  The class has the same owner as the first function in `fns`.
   *  Its position is the union of all functions in `fns`.
   */
  def AnonClass(parents: List[Type], fns: List[TermSymbol], methNames: List[TermName])(using Context): Block = {
    val owner = fns.head.owner
    val parents1 =
      if (parents.head.classSymbol.is(Trait)) {
        val head = parents.head.parents.head
        if (head.isRef(defn.AnyClass)) defn.AnyRefType :: parents else head :: parents
      }
      else parents
    val cls = newNormalizedClassSymbol(owner, tpnme.ANON_CLASS, Synthetic | Final, parents1,
        coord = fns.map(_.span).reduceLeft(_ union _))
    val constr = newConstructor(cls, Synthetic, Nil, Nil).entered
    def forwarder(fn: TermSymbol, name: TermName) = {
      val fwdMeth = fn.copy(cls, name, Synthetic | Method | Final).entered.asTerm
      if (fwdMeth.allOverriddenSymbols.exists(!_.is(Deferred))) fwdMeth.setFlag(Override)
      polyDefDef(fwdMeth, tprefs => prefss => ref(fn).appliedToTypes(tprefs).appliedToArgss(prefss))
    }
    val forwarders = fns.lazyZip(methNames).map(forwarder)
    val cdef = ClassDef(cls, DefDef(constr), forwarders.toLst)
    Block(Lst(cdef), New(cls.typeRef, NIL))
  }

  def Import(expr: Tree, selectors: Lst[untpd.ImportSelector])(using Context): Import =
    ta.assignType(untpd.Import(expr, selectors), newImportSymbol(ctx.owner, expr))

  def PackageDef(pid: RefTree, stats: Lst[Tree])(using Context): PackageDef =
    ta.assignType(untpd.PackageDef(pid, stats), pid)

  def Annotated(arg: Tree, annot: Tree)(using Context): Annotated =
    ta.assignType(untpd.Annotated(arg, annot), arg, annot)

  def Throw(expr: Tree)(using Context): Tree =
    ref(defn.throwMethod).appliedTo(expr)

  // ------ Making references ------------------------------------------------------

  def prefixIsElidable(tp: NamedType)(using Context): Boolean = {
    val typeIsElidable = tp.prefix match {
      case pre: ThisType =>
        tp.isType ||
        pre.cls.isStaticOwner ||
        tp.symbol.isParamOrAccessor && !pre.cls.is(Trait) && ctx.owner.enclosingClass == pre.cls
          // was ctx.owner.enclosingClass.derivesFrom(pre.cls) which was not tight enough
          // and was spuriously triggered in case inner class would inherit from outer one
          // eg anonymous TypeMap inside TypeMap.andThen
      case pre: TermRef =>
        pre.symbol.is(Module) && pre.symbol.isStatic
      case pre =>
        pre `eq` NoPrefix
    }
    typeIsElidable ||
    tp.symbol.is(JavaStatic) ||
    tp.symbol.hasAnnotation(defn.ScalaStaticAnnot)
  }

  def needsSelect(tp: Type)(using Context): Boolean = tp match {
    case tp: TermRef => !prefixIsElidable(tp)
    case _ => false
  }

  /** A tree representing the same reference as the given type */
  def ref(tp: NamedType)(using Context): Tree =
    if (tp.isType) TypeTree(tp)
    else if (prefixIsElidable(tp)) Ident(tp)
    else if (tp.symbol.is(Module) && ctx.owner.isContainedIn(tp.symbol.moduleClass))
      followOuterLinks(This(tp.symbol.moduleClass.asClass))
    else if (tp.symbol hasAnnotation defn.ScalaStaticAnnot)
      Ident(tp)
    else {
      val pre = tp.prefix
      if (pre.isSingleton) followOuterLinks(singleton(pre.dealias)).select(tp)
      else Select(TypeTree(pre), tp)
    }

  def ref(sym: Symbol)(using Context): Tree =
    ref(NamedType(sym.owner.thisType, sym.name, sym.denot))

  private def followOuterLinks(t: Tree)(using Context) = t match {
    case t: This if ctx.erasedTypes && !(t.symbol == ctx.owner.enclosingClass || t.symbol.isStaticOwner) =>
      // after erasure outer paths should be respected
      ExplicitOuter.OuterOps(ctx).path(toCls = t.tpe.classSymbol)
    case t =>
      t
  }

  def singleton(tp: Type)(using Context): Tree = tp.dealias match {
    case tp: TermRef => ref(tp)
    case tp: ThisType => This(tp.cls)
    case tp: SkolemType => singleton(tp.narrow)
    case SuperType(qual, _) => singleton(qual)
    case ConstantType(value) => Literal(value)
  }

  /** A path that corresponds to the given type `tp`. Error if `tp` is not a refinement
   *  of an addressable singleton type.
   */
  def pathFor(tp: Type)(using Context): Tree = {
    def recur(tp: Type): Tree = tp match {
      case tp: NamedType =>
        tp.info match {
          case TypeAlias(alias) => recur(alias)
          case _: TypeBounds => EmptyTree
          case _ => singleton(tp)
        }
      case tp: TypeProxy => recur(tp.superType)
      case _ => EmptyTree
    }
    recur(tp).orElse {
      report.error(em"$tp is not an addressable singleton type")
      TypeTree(tp)
    }
  }

  /** A tree representing a `newXYZArray` operation of the right
   *  kind for the given element type in `elemTpe`. No type arguments or
   *  `length` arguments are given.
   */
  def newArray(elemTpe: Type, returnTpe: Type, span: Span, dims: JavaSeqLiteral)(using Context): Tree = {
    val elemClass = elemTpe.classSymbol
    def newArr =
      ref(defn.DottyArraysModule).select(defn.newArrayMethod).withSpan(span)

    if (!ctx.erasedTypes) {
      assert(!TypeErasure.isGeneric(elemTpe), elemTpe) //needs to be done during typer. See Applications.convertNewGenericArray
      newArr.appliedToTypeTrees(Lst(TypeTree(returnTpe))).appliedToArgs(Lst(clsOf(elemTpe), clsOf(returnTpe), dims)).withSpan(span)
    }
    else  // after erasure
      newArr.appliedToArgs(Lst(clsOf(elemTpe), clsOf(returnTpe), dims)).withSpan(span)
  }

  /** The wrapped array method name for an array of type elemtp */
  def wrapArrayMethodName(elemtp: Type)(using Context): TermName = {
    val elemCls = elemtp.classSymbol
    if (elemCls.isPrimitiveValueClass) nme.wrapXArray(elemCls.name)
    else if (elemCls.derivesFrom(defn.ObjectClass) && !elemCls.isNotRuntimeClass) nme.wrapRefArray
    else nme.genericWrapArray
  }

  /** A tree representing a `wrapXYZArray(tree)` operation of the right
   *  kind for the given element type in `elemTpe`.
   */
  def wrapArray(tree: Tree, elemtp: Type)(using Context): Tree =
    val wrapper = ref(defn.getWrapVarargsArrayModule)
      .select(wrapArrayMethodName(elemtp))
      .appliedToTypes(if (elemtp.isPrimitiveValueType) Nil else elemtp :: Nil)
    val actualElem = wrapper.tpe.widen.firstParamTypes.head
    wrapper.appliedTo(tree.ensureConforms(actualElem))

  // ------ Creating typed equivalents of trees that exist only in untyped form -------

  /** new C(args), calling the primary constructor of C */
  def New(tp: Type, args: Lst[Tree])(using Context): Apply =
    New(tp, tp.dealias.typeSymbol.primaryConstructor.asTerm, args)

  /** new C(args), calling given constructor `constr` of C */
  def New(tp: Type, constr: TermSymbol, args: Lst[Tree])(using Context): Apply = {
    val targs = tp.argTypes
    val tycon = tp.typeConstructor
    New(tycon)
      .select(TermRef(tycon, constr))
      .appliedToTypes(targs)
      .appliedToArgs(args)
  }

  /** An object def
   *
   *     object obs extends parents { decls }
   *
   *  gets expanded to
   *
   *     <module> val obj = new obj$
   *     <module> class obj$ extends parents { this: obj.type => decls }
   *
   *  (The following no longer applies:
   *  What's interesting here is that the block is well typed
   *  (because class obj$ is hoistable), but the type of the `obj` val is
   *  not expressible. What needs to happen in general when
   *  inferring the type of a val from its RHS, is: if the type contains
   *  a class that has the val itself as owner, then that class
   *  is remapped to have the val's owner as owner. Remapping could be
   *  done by cloning the class with the new owner and substituting
   *  everywhere in the tree. We know that remapping is safe
   *  because the only way a local class can appear in the RHS of a val is
   *  by being hoisted outside of a block, and the necessary checks are
   *  done at this point already.
   *
   *  On the other hand, for method result type inference, if the type of
   *  the RHS of a method contains a class owned by the method, this would be
   *  an error.)
   */
  def ModuleDef(sym: TermSymbol, body: Lst[Tree])(using Context): tpd.Thicket = {
    val modcls = sym.moduleClass.asClass
    val constrSym = modcls.primaryConstructor orElse newDefaultConstructor(modcls).entered
    val constr = DefDef(constrSym.asTerm, EmptyTree)
    val clsdef = ClassDef(modcls, constr, body)
    val valdef = ValDef(sym, New(modcls.typeRef).select(constrSym).appliedToNone)
    Thicket(valdef, clsdef)
  }

  /** A `_` with given type */
  def Underscore(tp: Type)(using Context): Ident = untpd.Ident(nme.WILDCARD).withType(tp)

  def defaultValue(tpe: Type)(using Context): Tree = {
    val tpw = tpe.widen

    if (tpw isRef defn.IntClass) Literal(Constant(0))
    else if (tpw isRef defn.LongClass) Literal(Constant(0L))
    else if (tpw isRef defn.BooleanClass) Literal(Constant(false))
    else if (tpw isRef defn.CharClass) Literal(Constant('\u0000'))
    else if (tpw isRef defn.FloatClass) Literal(Constant(0f))
    else if (tpw isRef defn.DoubleClass) Literal(Constant(0d))
    else if (tpw isRef defn.ByteClass) Literal(Constant(0.toByte))
    else if (tpw isRef defn.ShortClass) Literal(Constant(0.toShort))
    else nullLiteral.select(defn.Any_asInstanceOf).appliedToType(tpe)
  }

  private class FindLocalDummyAccumulator(cls: ClassSymbol)(using Context) extends TreeAccumulator[Symbol] {
    def apply(sym: Symbol, tree: Tree)(using Context) =
      if (sym.exists) sym
      else if (tree.isDef) {
        val owner = tree.symbol.owner
        if (owner.isLocalDummy && owner.owner == cls) owner
        else if (owner == cls) foldOver(sym, tree)
        else sym
      }
      else foldOver(sym, tree)
  }

  override val cpy: TypedTreeCopier = // Type ascription needed to pick up any new members in TreeCopier (currently there are none)
    TypedTreeCopier()

  val cpyBetweenPhases: TimeTravellingTreeCopier = TimeTravellingTreeCopier()

  class TypedTreeCopier extends TreeCopier {
    def postProcess(tree: Tree, copied: untpd.Tree): copied.ThisTree[Type] =
      copied.withTypeUnchecked(tree.tpe)
    def postProcess(tree: Tree, copied: untpd.MemberDef): copied.ThisTree[Type] =
      copied.withTypeUnchecked(tree.tpe)

    protected val untpdCpy = untpd.cpy

    override def Select(tree: Tree)(qualifier: Tree, name: Name)(using Context): Select = {
      val tree1 = untpdCpy.Select(tree)(qualifier, name)
      tree match {
        case tree: Select if qualifier.tpe eq tree.qualifier.tpe =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ =>
          val tree2: Select = tree.tpe match {
            case tpe: NamedType =>
              val qualType = qualifier.tpe.widenIfUnstable
              if qualType.isBottomType then tree1.withTypeUnchecked(tree.tpe)
              else tree1.withType(tpe.derivedSelect(qualType))
            case _ => tree1.withTypeUnchecked(tree.tpe)
          }
          ConstFold.Select(tree2)
      }
    }

    override def Apply(tree: Tree)(fun: Tree, args: Lst[Tree])(using Context): Apply = {
      val tree1 = untpdCpy.Apply(tree)(fun, args)
      tree match {
        case tree: Apply
        if (fun.tpe eq tree.fun.tpe) && sameTypes(args, tree.args) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, fun, args)
      }
    }

    override def TypeApply(tree: Tree)(fun: Tree, args: Lst[Tree])(using Context): TypeApply = {
      val tree1 = untpdCpy.TypeApply(tree)(fun, args)
      tree match {
        case tree: TypeApply
        if (fun.tpe eq tree.fun.tpe) && sameTypes(args, tree.args) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, fun, args)
      }
    }

    override def Literal(tree: Tree)(const: Constant)(using Context): Literal =
      ta.assignType(untpdCpy.Literal(tree)(const))

    override def New(tree: Tree)(tpt: Tree)(using Context): New =
      ta.assignType(untpdCpy.New(tree)(tpt), tpt)

    override def Typed(tree: Tree)(expr: Tree, tpt: Tree)(using Context): Typed =
      ta.assignType(untpdCpy.Typed(tree)(expr, tpt), tpt)

    override def NamedArg(tree: Tree)(name: Name, arg: Tree)(using Context): NamedArg =
      ta.assignType(untpdCpy.NamedArg(tree)(name, arg), arg)

    override def Assign(tree: Tree)(lhs: Tree, rhs: Tree)(using Context): Assign =
      ta.assignType(untpdCpy.Assign(tree)(lhs, rhs))

    override def Block(tree: Tree)(stats: Lst[Tree], expr: Tree)(using Context): Block = {
      val tree1 = untpdCpy.Block(tree)(stats, expr)
      tree match {
        case tree: Block if (expr.tpe eq tree.expr.tpe) && (expr.tpe eq tree.tpe) =>
          // The last guard is a conservative check: if `tree.tpe` is different from `expr.tpe`, then
          // it was computed from widening `expr.tpe`, and tree transforms might cause `expr.tpe.widen`
          // to change even if `expr.tpe` itself didn't change, e.g:
          //     { val s = ...;  s }
          // If the type of `s` changed, then the type of the block might have changed, even though `expr.tpe`
          // will still be `TermRef(NoPrefix, s)`
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, stats, expr)
      }
    }

    override def If(tree: Tree)(cond: Tree, thenp: Tree, elsep: Tree)(using Context): If = {
      val tree1 = untpdCpy.If(tree)(cond, thenp, elsep)
      tree match {
        case tree: If if (thenp.tpe eq tree.thenp.tpe) && (elsep.tpe eq tree.elsep.tpe) &&
          ((tree.tpe eq thenp.tpe) || (tree.tpe eq elsep.tpe)) =>
          // The last guard is a conservative check similar to the one done in `Block` above,
          // if `tree.tpe` is not identical to the type of one of its branch, it might have been
          // computed from the widened type of the branches, so the same reasoning than
          // in `Block` applies.
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, thenp, elsep)
      }
    }

    override def Closure(tree: Tree)(env: Lst[Tree], meth: Tree, tpt: Tree)(using Context): Closure = {
      val tree1 = untpdCpy.Closure(tree)(env, meth, tpt)
      tree match {
        case tree: Closure if sameTypes(env, tree.env) && (meth.tpe eq tree.meth.tpe) && (tpt.tpe eq tree.tpt.tpe) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, meth, tpt)
      }
    }

    override def Match(tree: Tree)(selector: Tree, cases: Lst[CaseDef])(using Context): Match = {
      val tree1 = untpdCpy.Match(tree)(selector, cases)
      tree match {
        case tree: Match if sameTypes(cases, tree.cases) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, selector, cases)
      }
    }

    override def CaseDef(tree: Tree)(pat: Tree, guard: Tree, body: Tree)(using Context): CaseDef = {
      val tree1 = untpdCpy.CaseDef(tree)(pat, guard, body)
      tree match {
        case tree: CaseDef if body.tpe eq tree.body.tpe => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, pat, body)
      }
    }

    override def Labeled(tree: Tree)(bind: Bind, expr: Tree)(using Context): Labeled =
      ta.assignType(untpdCpy.Labeled(tree)(bind, expr))

    override def Return(tree: Tree)(expr: Tree, from: Tree)(using Context): Return =
      ta.assignType(untpdCpy.Return(tree)(expr, from))

    override def WhileDo(tree: Tree)(cond: Tree, body: Tree)(using Context): WhileDo =
      ta.assignType(untpdCpy.WhileDo(tree)(cond, body))

    override def Try(tree: Tree)(expr: Tree, cases: Lst[CaseDef], finalizer: Tree)(using Context): Try = {
      val tree1 = untpdCpy.Try(tree)(expr, cases, finalizer)
      tree match {
        case tree: Try if (expr.tpe eq tree.expr.tpe) && sameTypes(cases, tree.cases) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, expr, cases)
      }
    }

    override def Inlined(tree: Tree)(call: Tree, bindings: Lst[MemberDef], expansion: Tree)(using Context): Inlined = {
      val tree1 = untpdCpy.Inlined(tree)(call, bindings, expansion)
      tree match {
        case tree: Inlined if sameTypes(bindings, tree.bindings) && (expansion.tpe eq tree.expansion.tpe) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, bindings, expansion)
      }
    }

    override def SeqLiteral(tree: Tree)(elems: Lst[Tree], elemtpt: Tree)(using Context): SeqLiteral = {
      val tree1 = untpdCpy.SeqLiteral(tree)(elems, elemtpt)
      tree match {
        case tree: SeqLiteral
        if sameTypes(elems, tree.elems) && (elemtpt.tpe eq tree.elemtpt.tpe) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ =>
          ta.assignType(tree1, elems, elemtpt)
      }
    }

    override def Annotated(tree: Tree)(arg: Tree, annot: Tree)(using Context): Annotated = {
      val tree1 = untpdCpy.Annotated(tree)(arg, annot)
      tree match {
        case tree: Annotated if (arg.tpe eq tree.arg.tpe) && (annot eq tree.annot) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, arg, annot)
      }
    }

    override def If(tree: If)(cond: Tree = tree.cond, thenp: Tree = tree.thenp, elsep: Tree = tree.elsep)(using Context): If =
      If(tree: Tree)(cond, thenp, elsep)
    override def Closure(tree: Closure)(env: Lst[Tree] = tree.env, meth: Tree = tree.meth, tpt: Tree = tree.tpt)(using Context): Closure =
      Closure(tree: Tree)(env, meth, tpt)
    override def CaseDef(tree: CaseDef)(pat: Tree = tree.pat, guard: Tree = tree.guard, body: Tree = tree.body)(using Context): CaseDef =
      CaseDef(tree: Tree)(pat, guard, body)
    override def Try(tree: Try)(expr: Tree = tree.expr, cases: Lst[CaseDef] = tree.cases, finalizer: Tree = tree.finalizer)(using Context): Try =
      Try(tree: Tree)(expr, cases, finalizer)
  }

  class TimeTravellingTreeCopier extends TypedTreeCopier {
    override def Apply(tree: Tree)(fun: Tree, args: Lst[Tree])(using Context): Apply =
      tree match
        case tree: Apply
        if (tree.fun eq fun) && (tree.args eqLst args)
           && tree.tpe.isInstanceOf[ConstantType]
           && isPureExpr(tree) => tree
        case _ =>
          ta.assignType(untpdCpy.Apply(tree)(fun, args), fun, args)
      // Note: Reassigning the original type if `fun` and `args` have the same types as before
      // does not work here in general: The computed type depends on the widened function type, not
      // the function type itself. A tree transform may keep the function type the
      // same but its widened type might change.
      // However, we keep constant types of pure expressions. This uses the underlying assumptions
      // that pure functions yielding a constant will not change in later phases.

    override def TypeApply(tree: Tree)(fun: Tree, args: Lst[Tree])(using Context): TypeApply =
      ta.assignType(untpdCpy.TypeApply(tree)(fun, args), fun, args)
      // Same remark as for Apply

    override def Closure(tree: Tree)(env: Lst[Tree], meth: Tree, tpt: Tree)(using Context): Closure =
      ta.assignType(untpdCpy.Closure(tree)(env, meth, tpt), meth, tpt)

    override def Closure(tree: Closure)(env: Lst[Tree] = tree.env, meth: Tree = tree.meth, tpt: Tree = tree.tpt)(using Context): Closure =
      Closure(tree: Tree)(env, meth, tpt)
  }

  override def skipTransform(tree: Tree)(using Context): Boolean = tree.tpe.isError

  implicit class TreeOps[ThisTree <: tpd.Tree](private val tree: ThisTree) extends AnyVal {

    def isValue(using Context): Boolean =
      tree.isTerm && tree.tpe.widen.isValueType

    def isValueOrPattern(using Context): Boolean =
      tree.isValue || tree.isPattern

    def isValueType: Boolean =
      tree.isType && tree.tpe.isValueType

    def isInstantiation: Boolean = tree match {
      case Apply(Select(New(_), nme.CONSTRUCTOR), _) => true
      case _ => false
    }

    def shallowFold[T](z: T)(op: (T, tpd.Tree) => T)(using Context): T =
      ShallowFolder(op).apply(z, tree)

    def deepFold[T](z: T)(op: (T, tpd.Tree) => T)(using Context): T =
      DeepFolder(op).apply(z, tree)

    def find[T](pred: (tpd.Tree) => Boolean)(using Context): Option[tpd.Tree] =
      shallowFold[Option[tpd.Tree]](None)((accum, tree) => if (pred(tree)) Some(tree) else accum)

    def subst(from: List[Symbol], to: List[Symbol])(using Context): ThisTree =
      TreeTypeMap(substFrom = from, substTo = to).apply(tree)

    /** Change owner from `from` to `to`. If `from` is a weak owner, also change its
     *  owner to `to`, and continue until a non-weak owner is reached.
     */
    def changeOwner(from: Symbol, to: Symbol)(using Context): ThisTree = {
      @tailrec def loop(from: Symbol, froms: List[Symbol], tos: List[Symbol]): ThisTree =
        if (from.isWeakOwner && !from.owner.isClass)
          loop(from.owner, from :: froms, to :: tos)
        else
          //println(i"change owner ${from :: froms}%, % ==> $tos of $tree")
          TreeTypeMap(oldOwners = from :: froms, newOwners = tos).apply(tree)
      if (from == to) tree else loop(from, Nil, to :: Nil)
    }

    /**
     * Set the owner of every definition in this tree which is not itself contained in this
     * tree to be `newowner`
     */
    def changeNonLocalOwners(newOwner: Symbol)(using Context): Tree = {
      val ownerAcc = new TreeAccumulator[immutable.Set[Symbol]] {
        def apply(ss: immutable.Set[Symbol], tree: Tree)(using Context) = tree match {
          case tree: DefTree =>
            if (tree.symbol.exists) ss + tree.symbol.owner
            else ss
          case _ =>
            foldOver(ss, tree)
        }
      }
      val owners = ownerAcc(immutable.Set.empty[Symbol], tree).toList
      val newOwners = List.fill(owners.size)(newOwner)
      TreeTypeMap(oldOwners = owners, newOwners = newOwners).apply(tree)
    }

    /** After phase `trans`, set the owner of every definition in this tree that was formerly
     *  owner by `from` to `to`.
     */
    def changeOwnerAfter(from: Symbol, to: Symbol, trans: DenotTransformer)(using Context): ThisTree =
      if (ctx.phase == trans.next) {
        val traverser = new TreeTraverser {
          def traverse(tree: Tree)(using Context) = tree match {
            case tree: DefTree =>
              val sym = tree.symbol
              val prevDenot = atPhase(trans)(sym.denot)
              if (prevDenot.effectiveOwner == from.skipWeakOwner) {
                val d = sym.copySymDenotation(owner = to)
                d.installAfter(trans)
                d.transformAfter(trans, d => if (d.owner eq from) d.copySymDenotation(owner = to) else d)
              }
              if (sym.isWeakOwner) traverseChildren(tree)
            case _ =>
              traverseChildren(tree)
          }
        }
        traverser.traverse(tree)
        tree
      }
      else atPhase(trans.next)(changeOwnerAfter(from, to, trans))

    /** A select node with the given selector name and a computed type */
    def select(name: Name)(using Context): Select =
      Select(tree, name)

    /** A select node with the given selector name such that the designated
     *  member satisfies predicate `p`. Useful for disambiguating overloaded members.
     */
    def select(name: Name, p: Symbol => Boolean)(using Context): Select =
      select(tree.tpe.member(name).suchThat(p).symbol)

    /** A select node with the given type */
    def select(tp: NamedType)(using Context): Select =
      untpd.Select(tree, tp.name).withType(tp)

    /** A select node that selects the given symbol. Note: Need to make sure this
     *  is in fact the symbol you would get when you select with the symbol's name,
     *  otherwise a data race may occur which would be flagged by -Yno-double-bindings.
     */
    def select(sym: Symbol)(using Context): Select = {
      val tp =
        if (sym.isType) {
          assert(!sym.is(TypeParam))
          TypeRef(tree.tpe, sym.asType)
        }
        else
          TermRef(tree.tpe, sym.name.asTermName, sym.denot.asSeenFrom(tree.tpe))
      untpd.Select(tree, sym.name).withType(tp)
    }

    /** A select node with the given selector name and signature and a computed type */
    def selectWithSig(name: Name, sig: Signature)(using Context): Tree =
      untpd.SelectWithSig(tree, name, sig).withType(tree.tpe.select(name.asTermName, sig))

    /** A select node with selector name and signature taken from `sym`.
     *  Note: Use this method instead of select(sym) if the referenced symbol
     *  might be overridden in the type of the qualifier prefix. See note
     *  on select(sym: Symbol).
     */
    def selectWithSig(sym: Symbol)(using Context): Tree =
      selectWithSig(sym.name, sym.signature)

    /** A unary apply node with given argument: `tree(arg)` */
    def appliedTo(arg: Tree)(using Context): Apply =
      appliedToArgs(Lst(arg))

    /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
    def appliedTo(arg: Tree, args: Tree*)(using Context): Apply =
      appliedToArgs(arg :: args.toLst)

    /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
    def appliedToArgs(args: Lst[Tree])(using Context): Apply =
      Apply(tree, args)

    /** An applied node that accepts only varargs as arguments */
    def appliedToVarargs(args: List[Tree], tpt: Tree)(using Context): Apply =
      appliedTo(repeated(args, tpt))

    /** The current tree applied to given argument lists:
     *  `tree (argss(0)) ... (argss(argss.length -1))`
     */
    def appliedToArgss(argss: List[List[Tree]])(using Context): Tree =
      argss.foldLeft(tree: Tree)((fn, args) => Apply(fn, args.toLst))

    /** The current tree applied to given argument lists:
     *  `tree (argss(0)) ... (argss(argss.length -1))`
     */
    def appliedToArgssLst(argss: List[Lst[Tree]])(using Context): Tree =
      argss.foldLeft(tree: Tree)((fn, args) => Apply(fn, args))

    /** The current tree applied to (): `tree()` */
    def appliedToNone(using Context): Apply = appliedToArgs(NIL)

    /** The current tree applied to given type argument: `tree[targ]` */
    def appliedToType(targ: Type)(using Context): Tree =
      appliedToTypes(targ :: Nil)

    /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
    def appliedToTypes(targs: List[Type])(using Context): Tree =
      appliedToTypeTrees(targs.toLst.map(TypeTree(_)))

    /** The current tree applied to given type argument: `tree[targ]` */
    def appliedToTypeTree(targ: Tree)(using Context): Tree =
      appliedToTypeTrees(Lst(targ))

    /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
    def appliedToTypeTrees(targs: Lst[Tree])(using Context): Tree =
      if (targs.isEmpty) tree else TypeApply(tree, targs)

    /** Apply to `()` unless tree's widened type is parameterless */
    def ensureApplied(using Context): Tree =
      if (tree.tpe.widen.isParameterless) tree else tree.appliedToNone

    /** `tree == that` */
    def equal(that: Tree)(using Context): Tree =
      if (that.tpe.widen.isRef(defn.NothingClass))
        Literal(Constant(false))
      else
        applyOverloaded(tree, nme.EQ, Lst(that), Nil, defn.BooleanType)

    /** `tree.isInstanceOf[tp]`, with special treatment of singleton types */
    def isInstance(tp: Type)(using Context): Tree = tp.dealias match {
      case tp: SingletonType =>
        if (tp.widen.derivesFrom(defn.ObjectClass))
          tree.ensureConforms(defn.ObjectType).select(defn.Object_eq).appliedTo(singleton(tp))
        else
          singleton(tp).equal(tree)
      case _ =>
        tree.select(defn.Any_isInstanceOf).appliedToType(tp)
    }

    /** tree.asInstanceOf[`tp`] */
    def asInstance(tp: Type)(using Context): Tree = {
      assert(tp.isValueType, i"bad cast: $tree.asInstanceOf[$tp]")
      tree.select(defn.Any_asInstanceOf).appliedToType(tp)
    }

    /** cast tree to `tp`, assuming no exception is raised, i.e the operation is pure */
    def cast(tp: Type)(using Context): Tree = {
      assert(tp.isValueType, i"bad cast: $tree.asInstanceOf[$tp]")
      tree.select(if (ctx.erasedTypes) defn.Any_asInstanceOf else defn.Any_typeCast)
        .appliedToType(tp)
    }

    /** cast `tree` to `tp` (or its box/unbox/cast equivalent when after
     *  erasure and value and non-value types are mixed),
     *  unless tree's type already conforms to `tp`.
     */
    def ensureConforms(tp: Type)(using Context): Tree =
      if (tree.tpe <:< tp) tree
      else if (!ctx.erasedTypes) cast(tp)
      else Erasure.Boxing.adaptToType(tree, tp)

    /** `tree ne null` (might need a cast to be type correct) */
    def testNotNull(using Context): Tree = {
      val receiver = if (defn.isBottomType(tree.tpe))
        // If the receiver is of type `Nothing` or `Null`, add an ascription so that the selection
        // succeeds: e.g. `null.ne(null)` doesn't type, but `(null: AnyRef).ne(null)` does.
        Typed(tree, TypeTree(defn.AnyRefType))
      else tree.ensureConforms(defn.ObjectType)
      receiver.select(defn.Object_ne).appliedTo(nullLiteral).withSpan(tree.span)
    }

    /** If inititializer tree is `_`, the default value of its type,
     *  otherwise the tree itself.
     */
    def wildcardToDefault(using Context): Tree =
      if (isWildcardArg(tree)) defaultValue(tree.tpe) else tree

    /** `this && that`, for boolean trees `this`, `that` */
    def and(that: Tree)(using Context): Tree =
      tree.select(defn.Boolean_&&).appliedTo(that)

    /** `this || that`, for boolean trees `this`, `that` */
    def or(that: Tree)(using Context): Tree =
      tree.select(defn.Boolean_||).appliedTo(that)

    /** The translation of `tree = rhs`.
     *  This is either the tree as an assignment, or a setter call.
     */
    def becomes(rhs: Tree)(using Context): Tree = {
      val sym = tree.symbol
      if (sym.is(Method)) {
        val setter = sym.setter.orElse {
          assert(sym.name.isSetterName && sym.info.firstParamTypes.nonEmpty, sym)
          sym
        }
        val qual = tree match {
          case id: Ident => desugarIdentPrefix(id)
          case Select(qual, _) => qual
        }
        qual.select(setter).appliedTo(rhs)
      }
      else Assign(tree, rhs)
    }

    /** tree @annot
     *
     *  works differently for type trees and term trees
     */
    def annotated(annot: Tree)(using Context): Tree =
      if (tree.isTerm)
        Typed(tree, TypeTree(AnnotatedType(tree.tpe.widenIfUnstable, Annotation(annot))))
      else
        Annotated(tree, annot)

    /** A synthetic select with that will be turned into an outer path by ExplicitOuter.
     *  @param levels  How many outer levels to select
     *  @param tp      The type of the destination of the outer path.
     */
    def outerSelect(levels: Int, tp: Type)(using Context): Tree =
      untpd.Select(tree, OuterSelectName(EmptyTermName, levels)).withType(SkolemType(tp))

    /** Replace Inlined nodes and InlineProxy references to underlying arguments */
    def underlyingArgument(using Context): Tree = {
      val mapToUnderlying = new MapToUnderlying {
        /** Should get the rhs of this binding
         *  Returns true if the symbol is a val or def generated by eta-expansion/inline
         */
        override protected def skipLocal(sym: Symbol): Boolean =
          sym.isOneOf(InlineProxy | Synthetic)
      }
      mapToUnderlying.transform(tree)
    }

    /** Replace Ident nodes references to the underlying tree that defined them */
    def underlying(using Context): Tree = MapToUnderlying().transform(tree)

    // --- Higher order traversal methods -------------------------------

    /** Apply `f` to each subtree of this tree */
    def foreachSubTree(f: Tree => Unit)(using Context): Unit = {
      val traverser = new TreeTraverser {
        def traverse(tree: Tree)(using Context) = foldOver(f(tree), tree)
      }
      traverser.traverse(tree)
    }

    /** Is there a subtree of this tree that satisfies predicate `p`? */
    def existsSubTree(p: Tree => Boolean)(using Context): Boolean = {
      val acc = new TreeAccumulator[Boolean] {
        def apply(x: Boolean, t: Tree)(using Context) = x || p(t) || foldOver(x, t)
      }
      acc(false, tree)
    }

    /** All subtrees of this tree that satisfy predicate `p`. */
    def filterSubTrees(f: Tree => Boolean)(using Context): List[Tree] = {
      val buf = mutable.ListBuffer[Tree]()
      foreachSubTree { tree => if (f(tree)) buf += tree }
      buf.toList
    }

    /** Set this tree as the `defTree` of its symbol and return this tree */
    def setDefTree(using Context): ThisTree = {
      val sym = tree.symbol
      if (sym.exists) sym.defTree = tree
      tree
    }
  }

  inline val MapRecursionLimit = 10

  extension (trees: Lst[Tree])

    /** A map that expands to a recursive function. It's equivalent to
     *
     *    flatten(trees.mapConserve(op))
     *
     *  and falls back to it after `MaxRecursionLimit` recursions.
     *  Before that it uses a simpler method that uses stackspace
     *  instead of heap.
     *  Note `op` is duplicated in the generated code, so it should be
     *  kept small.
     */
    inline def mapInline(inline op: Tree => Tree): Lst[Tree] =
      flatten(trees.mapConserve(op))
  end extension

  /** Map Inlined nodes, NamedArgs, Blocks with no statements and local references to underlying arguments.
   *  Also drops Inline and Block with no statements.
   */
  class MapToUnderlying extends TreeMap {
    override def transform(tree: Tree)(using Context): Tree = tree match {
      case tree: Ident if isBinding(tree.symbol) && skipLocal(tree.symbol) =>
        tree.symbol.defTree match {
          case defTree: ValOrDefDef =>
            val rhs = defTree.rhs
            assert(!rhs.isEmpty)
            transform(rhs)
          case _ => tree
        }
      case Inlined(_, NIL, arg) => transform(arg)
      case Block(NIL, arg) => transform(arg)
      case NamedArg(_, arg) => transform(arg)
      case tree => super.transform(tree)
    }

    /** Should get the rhs of this binding */
    protected def skipLocal(sym: Symbol): Boolean = true

    /** Is this a symbol that of a local val or parameterless def for which we could get the rhs */
    private def isBinding(sym: Symbol)(using Context): Boolean =
      sym.isTerm && !sym.is(Param) && !sym.owner.isClass &&
      !(sym.is(Method) && sym.info.isInstanceOf[MethodOrPoly]) // if is a method it is parameterless
  }

  extension (xs: List[tpd.Tree]):
    def tpes: List[Type] = xs match {
      case x :: xs1 => x.tpe :: xs1.tpes
      case nil => Nil
    }

  extension (xs: Lst[tpd.Tree]):
    def tpes: List[Type] = xs.length match
      case 0 => Nil
      case 1 => xs(0).tpe :: Nil
      case 2 => xs(0).tpe :: xs(1).tpe :: Nil
      case 3 => xs(0).tpe :: xs(1).tpe :: xs(2).tpe :: Nil
      case _ =>
        val buf = new mutable.ListBuffer[Type]()
        xs.foreachInlined(x => buf += x.tpe)
        buf.toList

  /** A trait for loaders that compute trees. Currently implemented just by DottyUnpickler. */
  trait TreeProvider {
    protected def computeRootTrees(using Context): List[Tree]

    private var myTrees: List[Tree] = null

    /** Get trees defined by this provider. Cache them if -Yretain-trees is set. */
    def rootTrees(using Context): List[Tree] =
      if (ctx.settings.YretainTrees.value) {
        if (myTrees == null) myTrees = computeRootTrees
        myTrees
      }
      else computeRootTrees

    /** Get first tree defined by this provider, or EmptyTree if none exists */
    def tree(using Context): Tree =
      rootTrees.headOption.getOrElse(EmptyTree)

    /** Is it possible that the tree to load contains a definition of or reference to `id`? */
    def mightContain(id: String)(using Context): Boolean = true
  }

  // convert a numeric with a toXXX method
  def primitiveConversion(tree: Tree, numericCls: Symbol)(using Context): Tree = {
    val mname      = "to".concat(numericCls.name)
    val conversion = tree.tpe member(mname)
    if (conversion.symbol.exists)
      tree.select(conversion.symbol.termRef).ensureApplied
    else if (tree.tpe.widen isRef numericCls)
      tree
    else {
      report.warning(i"conversion from ${tree.tpe.widen} to ${numericCls.typeRef} will always fail at runtime.")
      Throw(New(defn.ClassCastExceptionClass.typeRef, NIL)).withSpan(tree.span)
    }
  }

  /** A tree that corresponds to `Predef.classOf[$tp]` in source */
  def clsOf(tp: Type)(using Context): Tree =
    if ctx.erasedTypes && !tp.isRef(defn.UnitClass) then
      Literal(Constant(TypeErasure.erasure(tp)))
    else
      Literal(Constant(tp))

  @tailrec
  def sameTypes(trees: List[tpd.Tree], trees1: List[tpd.Tree]): Boolean =
    if (trees.isEmpty) trees1.isEmpty
    else if (trees1.isEmpty) trees.isEmpty
    else (trees.head.tpe eq trees1.head.tpe) && sameTypes(trees.tail, trees1.tail)

  def sameTypes(ts1: Lst[Tree], ts2: Lst[Tree]): Boolean =
    (ts1 eqLst ts2) || ts1.corresponds(ts2)((t1, t2) => t1.tpe eq t2.tpe)

  /** If `tree`'s purity level is less than `level`, let-bind it so that it gets evaluated
   *  only once. I.e. produce a
   *
   *     { val x = 'tree ;  ~within('x) }
   *
   *  instead of otherwise
   *
   *     ~within('tree)
   */
  def letBindUnless(level: TreeInfo.PurityLevel, tree: Tree)(within: Tree => Tree)(using Context): Tree =
    if (exprPurity(tree) >= level) within(tree)
    else {
      val vdef = SyntheticValDef(TempResultName.fresh(), tree)
      Block(Lst(vdef), within(Ident(vdef.namedType)))
    }

  /** Let bind `tree` unless `tree` is at least idempotent */
  def evalOnce(tree: Tree)(within: Tree => Tree)(using Context): Tree =
    letBindUnless(TreeInfo.Idempotent, tree)(within)

  def runtimeCall(name: TermName, args: Lst[Tree])(using Context): Tree =
    Ident(defn.ScalaRuntimeModule.requiredMethod(name).termRef).appliedToArgs(args)

  /** An extractor that pulls out type arguments */
  object MaybePoly {
    def unapply(tree: Tree): Option[(Tree, Lst[Tree])] = tree match {
      case TypeApply(tree, targs) => Some(tree, targs)
      case _ => Some(tree, NIL)
    }
  }

  /** A key to be used in a context property that tracks enclosing inlined calls */
  private val InlinedCalls = Property.Key[List[Tree]]()

  /** A key to be used in a context property that tracks the number of inlined trees */
  private val InlinedTrees = Property.Key[Counter]()
  final class Counter {
    var count: Int = 0
  }

  /** Record an enclosing inlined call.
   *  EmptyTree calls (for parameters) cancel the next-enclosing call in the list instead of being added to it.
   *  We assume parameters are never nested inside parameters.
   */
  override def inlineContext(call: Tree)(using Context): Context = {
    // We assume enclosingInlineds is already normalized, and only process the new call with the head.
    val oldIC = enclosingInlineds

    val newIC =
      if call.isEmpty then
        oldIC match
          case t1 :: ts2 => ts2
          case _ => oldIC
      else
        call :: oldIC

    val ctx1 = ctx.fresh.setProperty(InlinedCalls, newIC)
    if oldIC.isEmpty then ctx1.setProperty(InlinedTrees, new Counter) else ctx1
  }

  /** All enclosing calls that are currently inlined, from innermost to outermost.
   */
  def enclosingInlineds(using Context): List[Tree] =
    ctx.property(InlinedCalls).getOrElse(Nil)

  /** Record inlined trees */
  def addInlinedTrees(n: Int)(using Context): Unit =
    ctx.property(InlinedTrees).foreach(_.count += n)

  /** Check if the limit on the number of inlined trees has been reached */
  def reachedInlinedTreesLimit(using Context): Boolean =
    ctx.property(InlinedTrees) match
      case Some(c) => c.count > ctx.settings.XmaxInlinedTrees.value
      case None => false

  /** The source file where the symbol of the `inline` method referred to by `call`
   *  is defined
   */
  def sourceFile(call: Tree)(using Context): SourceFile = call.symbol.source

  /** Desugar identifier into a select node. Return the tree itself if not possible */
  def desugarIdent(tree: Ident)(using Context): RefTree = {
    val qual = desugarIdentPrefix(tree)
    if (qual.isEmpty) tree
    else qual.select(tree.symbol)
  }

  /** Recover identifier prefix (e.g. this) if it exists */
  def desugarIdentPrefix(tree: Ident)(using Context): Tree = tree.tpe match {
    case TermRef(prefix: TermRef, _) =>
      ref(prefix)
    case TermRef(prefix: ThisType, _) =>
      This(prefix.cls)
    case _ =>
      EmptyTree
  }

  /**
   * The symbols that are imported with `expr.name`
   *
   * @param expr The base of the import statement
   * @param name The name that is being imported.
   * @return All the symbols that would be imported with `expr.name`.
   */
  def importedSymbols(expr: Tree, name: Name)(using Context): List[Symbol] = {
    def lookup(name: Name): Symbol = expr.tpe.member(name).symbol
    val symbols =
      List(lookup(name.toTermName),
           lookup(name.toTypeName),
           lookup(name.moduleClassName),
           lookup(name.sourceModuleName))

    symbols.map(_.sourceSymbol).filter(_.exists).distinct
  }

  /**
   * All the symbols that are imported by the first selector of `imp` that matches
   * `selectorPredicate`.
   *
   * @param imp The import statement to analyze
   * @param selectorPredicate A test to find the selector to use.
   * @return The symbols imported.
   */
  def importedSymbols(imp: Import,
                      selectorPredicate: untpd.ImportSelector => Boolean = util.common.alwaysTrue)
                     (using Context): List[Symbol] =
    imp.selectors.find(selectorPredicate) match
      case Some(sel) => importedSymbols(imp.expr, sel.name)
      case _ => Nil

  /**
   * The list of select trees that resolve to the same symbols as the ones that are imported
   * by `imp`.
   */
  def importSelections(imp: Import)(using Context): Lst[Select] = {
    def imported(sym: Symbol, id: untpd.Ident, rename: Option[untpd.Ident]): List[Select] = {
      // Give a zero-extent position to the qualifier to prevent it from being included several
      // times in results in the language server.
      val noPosExpr = focusPositions(imp.expr)
      val selectTree = Select(noPosExpr, sym.name).withSpan(id.span)
      rename match {
        case None =>
          selectTree :: Nil
        case Some(rename) =>
          // Get the type of the symbol that is actually selected, and construct a select
          // node with the new name and the type of the real symbol.
          val name = if (sym.name.isTypeName) rename.name.toTypeName else rename.name
          val actual = Select(noPosExpr, sym.name)
          val renameTree = Select(noPosExpr, name).withSpan(rename.span).withType(actual.tpe)
          selectTree :: renameTree :: Nil
      }
    }

    imp.selectors.flatMapIterable { sel =>
      if sel.isWildcard then Nil
      else
        val renamedOpt = sel.renamed match
          case renamed: untpd.Ident => Some(renamed)
          case untpd.EmptyTree => None
        importedSymbols(imp.expr, sel.name).flatMap { sym =>
          imported(sym, sel.imported, renamedOpt)
        }
    }
  }

  /** Creates the tuple type tree repesentation of the type trees in `ts` */
  def tupleTypeTree(elems: Lst[Tree])(using Context): Tree = {
    val arity = elems.length
    if (arity <= Definitions.MaxTupleArity && defn.TupleType(arity) != null) AppliedTypeTree(TypeTree(defn.TupleType(arity)), elems)
    else nestedPairsTypeTree(elems)
  }

  /** Creates the nested pairs type tree repesentation of the type trees in `ts` */
  def nestedPairsTypeTree(ts: Lst[Tree])(using Context): Tree =
    ts.foldRight(TypeTree(defn.EmptyTupleModule.termRef): Tree)((x, acc) => AppliedTypeTree(TypeTree(defn.PairClass.typeRef), Lst(x, acc)))

  /** Replaces all positions in `tree` with zero-extent positions */
  private def focusPositions(tree: Tree)(using Context): Tree = {
    val transformer = new tpd.TreeMap {
      override def transform(tree: Tree)(using Context): Tree =
        super.transform(tree).withSpan(tree.span.focus)
    }
    transformer.transform(tree)
  }

  /** Convert a list of trees to a vararg-compatible tree.
   *  Used to make arguments for methods that accept varargs.
   */
  def repeated(trees: List[Tree], tpt: Tree)(using Context): Tree =
    ctx.typeAssigner.arrayToRepeated(JavaSeqLiteral(trees.toLst, tpt))

  /** Create a tree representing a list containing all
   *  the elements of the argument list. A "list of tree to
   *  tree of list" conversion.
   *
   *  @param trees  the elements the list represented by
   *                the resulting tree should contain.
   *  @param tpe    the type of the elements of the resulting list.
   *
   */
  def mkList(trees: List[Tree], tpe: Tree)(using Context): Tree =
    ref(defn.ListModule).select(nme.apply)
      .appliedToTypeTree(tpe)
      .appliedToVarargs(trees, tpe)


  protected def FunProto(args: Lst[Tree], resType: Type)(using Context) =
    ProtoTypes.FunProtoTyped(args, resType)(ctx.typer, ApplyKind.Regular)
}
