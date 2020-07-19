package dottyBench.tools
package dotc
package typer

import core._
import util.Spans.Span
import Contexts._
import Types._, Flags._, Symbols._, Types._, Names._, StdNames._, Constants._
import TypeErasure.{erasure, hasStableErasure}
import Decorators._
import ProtoTypes._
import Inferencing.{fullyDefinedType, isFullyDefined}
import ast.untpd
import transform.SymUtils._
import transform.TypeUtils._
import transform.SyntheticMembers._
import util.Property
import annotation.{tailrec, constructorOnly}

/** Synthesize terms for special classes */
class Synthesizer(typer: Typer)(using @constructorOnly c: Ctx, cs: CState):
  import ast.tpd._

  /** Handlers to synthesize implicits for special types */
  type SpecialHandler = (Type, Span) => (Ctx, CState) ?=> Tree
  private type SpecialHandlers = List[(ClassSymbol, SpecialHandler)]

  val synthesizedClassTag: SpecialHandler = (formal, span) =>
    formal.argInfos match
      case arg :: Nil =>
        fullyDefinedType(arg, "ClassTag argument", span) match
          case defn.ArrayOf(elemTp) =>
            val etag = typer.inferImplicitArg(defn.ClassTagClass.typeRef.appliedTo(elemTp), span)
            if etag.tpe.isError then EmptyTree else etag.select(nme.wrap)
          case tp if hasStableErasure(tp) && !defn.isBottomClass(tp.typeSymbol) =>
            val sym = tp.typeSymbol
            val classTag = ref(defn.ClassTagModule)
            val tag =
              if sym == defn.UnitClass
                 || sym == defn.AnyClass
                 || sym == defn.AnyValClass
              then
                classTag.select(sym.name.toTermName)
              else
                classTag.select(nme.apply).appliedToType(tp).appliedTo(clsOf(erasure(tp)))
            tag.withSpan(span)
          case tp => EmptyTree
      case _ => EmptyTree
  end synthesizedClassTag

  /** Synthesize the tree for `'[T]` for an implicit `scala.quoted.Type[T]`.
   *  `T` is deeply dealiased to avoid references to local type aliases.
   */
  val synthesizedTypeTag: SpecialHandler = (formal, span) =>
    def quotedType(t: Type) =
      if StagingContext.level == 0 then
        ctx.compilationUnit.needsStaging = true // We will need to run ReifyQuotes
      val qctx = ctx.typer.inferImplicitArg(defn.QuoteContextClass.typeRef, span)
      qctx.tpe match
        case tpe: Implicits.SearchFailureType => report.error(tpe.msg, ctx.source.atSpan(span))
        case _ =>
      ref(defn.InternalQuoted_typeQuote).appliedToType(t).select(nme.apply).appliedTo(qctx)
    formal.argInfos match
      case arg :: Nil =>
        val deepDealias = new TypeMap:
          def apply(tp: Type): Type = mapOver(tp.dealias)
        quotedType(deepDealias(arg))
      case _ =>
        EmptyTree
  end synthesizedTypeTag

  val synthesizedTupleFunction: SpecialHandler = (formal, span) =>
    formal match
      case AppliedType(_, funArgs @ fun :: tupled :: Nil) =>
        def functionTypeEqual(baseFun: Type, actualArgs: List[Type],
            actualRet: Type, expected: Type) =
          expected =:= defn.FunctionOf(actualArgs, actualRet,
            defn.isContextFunctionType(baseFun), defn.isErasedFunctionType(baseFun))
        val arity: Int =
          if defn.isErasedFunctionType(fun) || defn.isErasedFunctionType(fun) then -1 // TODO support?
          else if defn.isFunctionType(fun) then
            // TupledFunction[(...) => R, ?]
            fun.dropDependentRefinement.dealias.argInfos match
              case funArgs :+ funRet
              if functionTypeEqual(fun, defn.tupleType(funArgs) :: Nil, funRet, tupled) =>
                // TupledFunction[(...funArgs...) => funRet, ?]
                funArgs.size
              case _ => -1
          else if defn.isFunctionType(tupled) then
            // TupledFunction[?, (...) => R]
            tupled.dropDependentRefinement.dealias.argInfos match
              case tupledArgs :: funRet :: Nil =>
                defn.tupleTypes(tupledArgs.dealias) match
                  case Some(funArgs) if functionTypeEqual(tupled, funArgs, funRet, fun) =>
                    // TupledFunction[?, ((...funArgs...)) => funRet]
                    funArgs.size
                  case _ => -1
              case _ => -1
          else
            // TupledFunction[?, ?]
            -1
        if arity == -1 then
          EmptyTree
        else if arity <= Definitions.MaxImplementedFunctionArity then
          ref(defn.InternalTupleFunctionModule)
            .select(s"tupledFunction$arity".toTermName)
            .appliedToTypes(funArgs)
        else
          ref(defn.InternalTupleFunctionModule)
            .select("tupledFunctionXXL".toTermName)
            .appliedToTypes(funArgs)
      case _ =>
        EmptyTree
  end synthesizedTupleFunction

  /** If `formal` is of the form Eql[T, U], try to synthesize an
    *  `Eql.eqlAny[T, U]` as solution.
    */
  val synthesizedEql: SpecialHandler = (formal, span) =>

    /** Is there an `Eql[T, T]` instance, assuming -strictEquality? */
    def hasEq(tp: Type)(using Ctx, CState): Boolean =
      val inst = typer.inferImplicitArg(defn.EqlClass.typeRef.appliedTo(tp, tp), span)
      !inst.isEmpty && !inst.tpe.isError

    /** Can we assume the eqlAny instance for `tp1`, `tp2`?
      *  This is the case if assumedCanEqual(tp1, tp2), or
      *  one of `tp1`, `tp2` has a reflexive `Eql` instance.
      */
    def validEqAnyArgs(tp1: Type, tp2: Type)(using Ctx, CState) =
      typer.assumedCanEqual(tp1, tp2)
       || withMode(Mode.StrictEquality) {
            !hasEq(tp1) && !hasEq(tp2)
          }

    /** Is an `Eql[cls1, cls2]` instance assumed for predefined classes `cls1`, cls2`? */
    def canComparePredefinedClasses(cls1: ClassSymbol, cls2: ClassSymbol): Boolean =

      def cmpWithBoxed(cls1: ClassSymbol, cls2: ClassSymbol) =
        cls2 == defn.boxedType(cls1.typeRef).symbol
        || cls1.isNumericValueClass && cls2.derivesFrom(defn.BoxedNumberClass)

      if cls1.isPrimitiveValueClass then
        if cls2.isPrimitiveValueClass then
          cls1 == cls2 || cls1.isNumericValueClass && cls2.isNumericValueClass
        else
          cmpWithBoxed(cls1, cls2)
      else if cls2.isPrimitiveValueClass then
        cmpWithBoxed(cls2, cls1)
      else if ctx.explicitNulls then
        // If explicit nulls is enabled, we want to disallow comparison between Object and Null.
        // If a nullable value has a non-nullable type, we can still cast it to nullable type
        // then compare.
        //
        // Example:
        // val x: String = null.asInstanceOf[String]
        // if (x == null) {} // error: x is non-nullable
        // if (x.asInstanceOf[String|Null] == null) {} // ok
        cls1 == defn.NullClass && cls1 == cls2
      else if cls1 == defn.NullClass then
        cls1 == cls2 || cls2.derivesFrom(defn.ObjectClass)
      else if cls2 == defn.NullClass then
        cls1.derivesFrom(defn.ObjectClass)
      else
        false
    end canComparePredefinedClasses

    /** Some simulated `Eql` instances for predefined types. It's more efficient
      *  to do this directly instead of setting up a lot of `Eql` instances to
      *  interpret.
      */
    def canComparePredefined(tp1: Type, tp2: Type) =
      tp1.classSymbols.exists(cls1 =>
        tp2.classSymbols.exists(cls2 =>
          canComparePredefinedClasses(cls1, cls2)))

    formal.argTypes match
      case args @ (arg1 :: arg2 :: Nil) =>
        List(arg1, arg2).foreach(fullyDefinedType(_, "eq argument", span))
        if canComparePredefined(arg1, arg2)
            || !Implicits.strictEquality && explore(validEqAnyArgs(arg1, arg2))
        then ref(defn.Eql_eqlAny).appliedToTypes(args).withSpan(span)
        else EmptyTree
      case _ => EmptyTree
  end synthesizedEql

  /** Creates a tree that will produce a ValueOf instance for the requested type.
   * An EmptyTree is returned if materialization fails.
   */
  val synthesizedValueOf: SpecialHandler = (formal, span) =>

    def success(t: Tree) =
      New(defn.ValueOfClass.typeRef.appliedTo(t.tpe), t :: Nil).withSpan(span)

    formal.argInfos match
      case arg :: Nil =>
        fullyDefinedType(arg.dealias, "ValueOf argument", span).normalized match
          case ConstantType(c: Constant) =>
            success(Literal(c))
          case TypeRef(_, sym) if sym == defn.UnitClass =>
            success(Literal(Constant(())))
          case n: TermRef =>
            success(ref(n))
          case tp =>
            EmptyTree
      case _ =>
        EmptyTree
  end synthesizedValueOf

  /** Create an anonymous class `new Object { type MirroredMonoType = ... }`
   *  and mark it with given attachment so that it is made into a mirror at PostTyper.
   */
  private def anonymousMirror(monoType: Type, attachment: Property.StickyKey[Unit], span: Span)(using Ctx, CState) =
    val monoTypeDef = untpd.TypeDef(tpnme.MirroredMonoType, untpd.TypeTree(monoType))
    val newImpl = untpd.Template(
      constr = untpd.emptyConstructor,
      parents = untpd.TypeTree(defn.ObjectType) :: Nil,
      derived = Nil,
      self = EmptyValDef,
      body = monoTypeDef :: Nil
    ).withAttachment(attachment, ())
    typer.typed(untpd.New(newImpl).withSpan(span))

  /** The mirror type
   *
   *     <parent> {
   *       MirroredMonoType = <monoType>
   *       MirroredType = <mirroredType>
   *       MirroredLabel = <label> }
   *     }
   */
  private def mirrorCore(parentClass: ClassSymbol, monoType: Type, mirroredType: Type, label: Name, formal: Type)(using Ctx, CState) =
    formal & parentClass.typeRef
      .refinedWith(tpnme.MirroredMonoType, TypeAlias(monoType))
      .refinedWith(tpnme.MirroredType, TypeAlias(mirroredType))
      .refinedWith(tpnme.MirroredLabel, TypeAlias(ConstantType(Constant(label.toString))))

  /** A path referencing the companion of class type `clsType` */
  private def companionPath(clsType: Type, span: Span)(using Ctx, CState) =
    val ref = pathFor(clsType.companionRef)
    assert(ref.symbol.is(Module) && (clsType.classSymbol.is(ModuleClass) || (ref.symbol.companionClass == clsType.classSymbol)))
    ref.withSpan(span)

  private def checkFormal(formal: Type)(using Ctx, CState): Boolean =
    @tailrec
    def loop(tp: Type): Boolean = tp match
      case RefinedType(parent, _, _: TypeBounds) => loop(parent)
      case RefinedType(_, _, _) => false
      case _ => true
    loop(formal)

  private def mkMirroredMonoType(mirroredType: HKTypeLambda)(using Ctx, CState): Type =
    val monoMap = new TypeMap:
      def apply(t: Type) = t match
        case TypeParamRef(lambda, n) if lambda eq mirroredType => mirroredType.paramInfos(n)
        case t => mapOver(t)
    monoMap(mirroredType.resultType)

  private def productMirror(mirroredType: Type, formal: Type, span: Span)(using Ctx, CState): Tree =
    mirroredType match
      case AndType(tp1, tp2) =>
        productMirror(tp1, formal, span).orElse(productMirror(tp2, formal, span))
      case _ =>
        if mirroredType.termSymbol.is(CaseVal) then
          val module = mirroredType.termSymbol
          val modulePath = pathFor(mirroredType).withSpan(span)
          if module.info.classSymbol.is(Scala2x) then
            val mirrorType = mirrorCore(defn.Mirror_SingletonProxyClass, mirroredType, mirroredType, module.name, formal)
            val mirrorRef = New(defn.Mirror_SingletonProxyClass.typeRef, modulePath :: Nil)
            mirrorRef.cast(mirrorType)
          else
            val mirrorType = mirrorCore(defn.Mirror_SingletonClass, mirroredType, mirroredType, module.name, formal)
            modulePath.cast(mirrorType)
        else if mirroredType.classSymbol.isGenericProduct then
          val cls = mirroredType.classSymbol
          val accessors = cls.caseAccessors.filterNot(_.isAllOf(PrivateLocal))
          val elemLabels = accessors.map(acc => ConstantType(Constant(acc.name.toString)))
          val (monoType, elemsType) = mirroredType match
            case mirroredType: HKTypeLambda =>
              def accessorType(acc: Symbol) =
                if cls.typeParams.hasSameLengthAs(mirroredType.paramRefs) then
                  acc.info.subst(cls.typeParams, mirroredType.paramRefs)
                else
                  acc.info
              val elems =
                mirroredType.derivedLambdaType(
                  resType = TypeOps.nestedPairs(accessors.map(accessorType))
                )
              (mkMirroredMonoType(mirroredType), elems)
            case _ =>
              val elems = TypeOps.nestedPairs(accessors.map(mirroredType.memberInfo(_).widenExpr))
              (mirroredType, elems)
          val mirrorType =
            mirrorCore(defn.Mirror_ProductClass, monoType, mirroredType, cls.name, formal)
              .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
              .refinedWith(tpnme.MirroredElemLabels, TypeAlias(TypeOps.nestedPairs(elemLabels)))
          val mirrorRef =
            if (cls.is(Scala2x)) anonymousMirror(monoType, ExtendsProductMirror, span)
            else companionPath(mirroredType, span)
          mirrorRef.cast(mirrorType)
        else EmptyTree
  end productMirror

  private def sumMirror(mirroredType: Type, formal: Type, span: Span)(using Ctx, CState): Tree =
    if mirroredType.classSymbol.isGenericSum then
      val cls = mirroredType.classSymbol
      val elemLabels = cls.children.map(c => ConstantType(Constant(c.name.toString)))

      def solve(sym: Symbol): Type = sym match
        case caseClass: ClassSymbol =>
          assert(caseClass.is(Case))
          if caseClass.is(Module) then
            caseClass.sourceModule.termRef
          else
            caseClass.primaryConstructor.info match
              case info: PolyType =>
                // Compute the the full child type by solving the subtype constraint
                // `C[X1, ..., Xn] <: P`, where
                //
                //   - P is the current `mirroredType`
                //   - C is the child class, with type parameters X1, ..., Xn
                //
                // Contravariant type parameters are minimized, all other type parameters are maximized.
                def instantiate(using Ctx, CState) =
                  val poly = constrained(info, untpd.EmptyTree)._1
                  val resType = poly.finalResultType
                  val target = mirroredType match
                    case tp: HKTypeLambda => tp.resultType
                    case tp => tp
                  resType <:< target
                  val tparams = poly.paramRefs
                  val variances = caseClass.typeParams.map(_.paramVarianceSign)
                  val instanceTypes = tparams.lazyZip(variances).map((tparam, variance) =>
                    ctx.typeComparer.instanceType(tparam, fromBelow = variance < 0))
                  resType.substParams(poly, instanceTypes)
                instantiate(using ctx.fresh.setExploreTyperState().setOwner(caseClass))
              case _ =>
                caseClass.typeRef
        case child => child.termRef
      end solve

      val (monoType, elemsType) = mirroredType match
        case mirroredType: HKTypeLambda =>
          val elems = mirroredType.derivedLambdaType(
            resType = TypeOps.nestedPairs(cls.children.map(solve))
          )
          (mkMirroredMonoType(mirroredType), elems)
        case _ =>
          val elems = TypeOps.nestedPairs(cls.children.map(solve))
          (mirroredType, elems)

      val mirrorType =
          mirrorCore(defn.Mirror_SumClass, monoType, mirroredType, cls.name, formal)
          .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
          .refinedWith(tpnme.MirroredElemLabels, TypeAlias(TypeOps.nestedPairs(elemLabels)))
      val mirrorRef =
        if cls.linkedClass.exists && !cls.is(Scala2x)
        then companionPath(mirroredType, span)
        else anonymousMirror(monoType, ExtendsSumMirror, span)
      mirrorRef.cast(mirrorType)
    else EmptyTree
  end sumMirror

  def makeMirror
      (synth: (Type, Type, Span) => (Ctx, CState) ?=> Tree, formal: Type, span: Span)
      (using Ctx, CState): Tree =
    if checkFormal(formal) then
      formal.member(tpnme.MirroredType).info match
        case TypeBounds(mirroredType, _) => synth(mirroredType.stripTypeVar, formal, span)
        case other => EmptyTree
    else EmptyTree

  /** An implied instance for a type of the form `Mirror.Product { type MirroredType = T }`
   *  where `T` is a generic product type or a case object or an enum case.
   */
  val synthesizedProductMirror: SpecialHandler = (formal, span) =>
    makeMirror(productMirror, formal, span)

  /** An implied instance for a type of the form `Mirror.Sum { type MirroredType = T }`
   *  where `T` is a generic sum type.
   */
  val synthesizedSumMirror: SpecialHandler = (formal, span) =>
    makeMirror(sumMirror, formal, span)

  /** An implied instance for a type of the form `Mirror { type MirroredType = T }`
   *  where `T` is a generic sum or product or singleton type.
   */
  val synthesizedMirror: SpecialHandler = (formal, span) =>
    formal.member(tpnme.MirroredType).info match
      case TypeBounds(mirroredType, _) =>
        if mirroredType.termSymbol.is(CaseVal)
           || mirroredType.classSymbol.isGenericProduct
        then
          synthesizedProductMirror(formal, span)
        else
          synthesizedSumMirror(formal, span)
      case _ => EmptyTree

  val specialHandlers = List(
    defn.ClassTagClass        -> synthesizedClassTag,
    defn.QuotedTypeClass      -> synthesizedTypeTag,
    defn.EqlClass             -> synthesizedEql,
    defn.TupledFunctionClass  -> synthesizedTupleFunction,
    defn.ValueOfClass         -> synthesizedValueOf,
    defn.Mirror_ProductClass  -> synthesizedProductMirror,
    defn.Mirror_SumClass      -> synthesizedSumMirror,
    defn.MirrorClass          -> synthesizedMirror)

  def tryAll(formal: Type, span: Span)(using Ctx, CState): Tree =
    def recur(handlers: SpecialHandlers): Tree = handlers match
      case (cls, handler) :: rest =>
        def baseWithRefinements(tp: Type): Type = tp.dealias match
          case tp @ RefinedType(parent, rname, rinfo) =>
            tp.derivedRefinedType(baseWithRefinements(parent), rname, rinfo)
          case _ =>
            tp.baseType(cls)
        val base = baseWithRefinements(formal)
        val result =
          if (base <:< formal.widenExpr)
            // With the subtype test we enforce that the searched type `formal` is of the right form
            handler(base, span)
          else EmptyTree
        result.orElse(recur(rest))
      case Nil =>
        EmptyTree
    recur(specialHandlers)

end Synthesizer