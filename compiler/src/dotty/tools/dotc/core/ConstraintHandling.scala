package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._
import Decorators._
import config.Config
import config.Printers.{constr, typr}
import TypeApplications.EtaExpansion
import collection.mutable

/** Methods for adding constraints and solving them.
 *
 * What goes into a Constraint as opposed to a ConstrainHandler?
 *
 * Constraint code is purely functional: Operations get constraints and produce new ones.
 * Constraint code does not have access to a type-comparer. Anything regarding lubs and glbs has to be done
 * elsewhere.
 *
 * By comparison: Constraint handlers are parts of type comparers and can use their functionality.
 * Constraint handlers update the current constraint as a side effect.
 */
trait ConstraintHandling {

  implicit val ctx: Context

  protected def isSubType(tp1: Type, tp2: Type): Boolean
  protected def isSameType(tp1: Type, tp2: Type): Boolean

  val state: TyperState
  import state.constraint

  private var addConstraintInvocations = 0

  /** If the constraint is frozen we cannot add new bounds to the constraint. */
  protected var frozenConstraint = false

  /** If set, align arguments `S1`, `S2`when taking the glb
   *  `T1 { X = S1 } & T2 { X = S2 }` of a constraint upper bound for some type parameter.
   *  Aligning means computing `S1 =:= S2` which may change the current constraint.
   *  See note in TypeComparer#distributeAnd.
   */
  protected var homogenizeArgs = false

  /** We are currently comparing type lambdas. Used as a flag for
   *  optimization: when `false`, no need to do an expensive `pruneLambdaParams`
   */
  protected var comparedTypeLambdas: Set[TypeLambda] = Set.empty

  private def addOneBound(param: TypeParamRef, bound: Type, isUpper: Boolean): Boolean =
    !constraint.contains(param) || {
      def occursIn(bound: Type): Boolean = {
        val b = bound.dealias
        (b eq param) || {
          b match {
            case b: AndOrType => occursIn(b.tp1) || occursIn(b.tp2)
            case b: TypeVar => occursIn(b.origin)
            case b: TermRef => occursIn(b.underlying)
            case _ => false
          }
        }
      }
      if (Config.checkConstraintsSeparated)
        assert(!occursIn(bound), s"$param occurs in $bound")
      val newBound = narrowedBound(param, bound, isUpper)
      val c1 = constraint.updateEntry(param, newBound)
      (c1 eq constraint) || {
        constraint = c1
        val TypeBounds(lo, hi) = constraint.entry(param)
        isSubType(lo, hi)
      }
    }

  /** Narrow one of the bounds of type parameter `param`
   *  If `isUpper` is true, ensure that `param <: `bound`, otherwise ensure
   *  that `param >: bound`.
   */
  def narrowedBound(param: TypeParamRef, bound: Type, isUpper: Boolean)(implicit ctx: Context): TypeBounds = {
    val oldBounds @ TypeBounds(lo, hi) = constraint.nonParamBounds(param)
    val saved = homogenizeArgs
    homogenizeArgs = Config.alignArgsInAnd
    try
      if (isUpper) oldBounds.derivedTypeBounds(lo, hi & bound)
      else oldBounds.derivedTypeBounds(lo | bound, hi)
    finally homogenizeArgs = saved
  }

  protected def addUpperBound(param: TypeParamRef, bound: Type): Boolean = {
    def description = i"constraint $param <: $bound to\n$constraint"
    if (bound.isRef(defn.NothingClass) && ctx.typerState.isGlobalCommittable) {
      def msg = s"!!! instantiated to Nothing: $param, constraint = ${constraint.show}"
      if (Config.failOnInstantiationToNothing) assert(false, msg)
      else ctx.log(msg)
    }
    constr.println(i"adding $description in ${ctx.typerState.hashesStr}")
    val lower = constraint.lower(param)
    val res =
      addOneBound(param, bound, isUpper = true) &&
      lower.forall(addOneBound(_, bound, isUpper = true))
    constr.println(i"added $description = $res in ${ctx.typerState.hashesStr}")
    res
  }

  protected def addLowerBound(param: TypeParamRef, bound: Type): Boolean = {
    def description = i"constraint $param >: $bound to\n$constraint"
    constr.println(i"adding $description")
    val upper = constraint.upper(param)
    val res =
      addOneBound(param, bound, isUpper = false) &&
      upper.forall(addOneBound(_, bound, isUpper = false))
    constr.println(i"added $description = $res in ${ctx.typerState.hashesStr}")
    res
  }

  protected def addLess(p1: TypeParamRef, p2: TypeParamRef): Boolean = {
    def description = i"ordering $p1 <: $p2 to\n$constraint"
    val res =
      if (constraint.isLess(p2, p1)) unify(p2, p1)
      else {
        val down1 = p1 :: constraint.exclusiveLower(p1, p2)
        val up2 = p2 :: constraint.exclusiveUpper(p2, p1)
        val lo1 = constraint.nonParamBounds(p1).lo
        val hi2 = constraint.nonParamBounds(p2).hi
        constr.println(i"adding $description down1 = $down1, up2 = $up2 ${ctx.typerState.hashesStr}")
        constraint = constraint.addLess(p1, p2)
        down1.forall(addOneBound(_, hi2, isUpper = true)) &&
        up2.forall(addOneBound(_, lo1, isUpper = false))
      }
    constr.println(i"added $description = $res ${ctx.typerState.hashesStr}")
    res
  }

  /** Make p2 = p1, transfer all bounds of p2 to p1
   *  @pre  less(p1)(p2)
   */
  private def unify(p1: TypeParamRef, p2: TypeParamRef): Boolean = {
    constr.println(s"unifying $p1 $p2")
    assert(constraint.isLess(p1, p2))
    val down = constraint.exclusiveLower(p2, p1)
    val up = constraint.exclusiveUpper(p1, p2)
    constraint = constraint.unify(p1, p2)
    val bounds = constraint.nonParamBounds(p1)
    val lo = bounds.lo
    val hi = bounds.hi
    isSubType(lo, hi) &&
    down.forall(addOneBound(_, hi, isUpper = true)) &&
    up.forall(addOneBound(_, lo, isUpper = false))
  }


  protected def isSubType(tp1: Type, tp2: Type, whenFrozen: Boolean): Boolean = {
    if (whenFrozen)
      isSubTypeWhenFrozen(tp1, tp2)
    else
      isSubType(tp1, tp2)
  }

  final def isSubTypeWhenFrozen(tp1: Type, tp2: Type): Boolean = {
    val saved = frozenConstraint
    frozenConstraint = true
    try isSubType(tp1, tp2)
    finally frozenConstraint = saved
  }

  final def isSameTypeWhenFrozen(tp1: Type, tp2: Type): Boolean = {
    val saved = frozenConstraint
    frozenConstraint = true
    try isSameType(tp1, tp2)
    finally frozenConstraint = saved
  }

  /** Test whether the lower bounds of all parameters in this
   *  constraint are a solution to the constraint.
   */
  protected final def isSatisfiable: Boolean =
    constraint.forallParams { param =>
      val TypeBounds(lo, hi) = constraint.entry(param)
      isSubType(lo, hi) || {
        ctx.log(i"sub fail $lo <:< $hi")
        false
      }
    }

  /** Solve constraint set for given type parameter `param`.
   *  If `fromBelow` is true the parameter is approximated by its lower bound,
   *  otherwise it is approximated by its upper bound. However, any occurrences
   *  of the parameter in a refinement somewhere in the bound are removed. Also
   *  wildcard types in bounds are approximated by their upper or lower bounds.
   *  (Such occurrences can arise for F-bounded types).
   *  The constraint is left unchanged.
   *  @return the instantiating type
   *  @pre `param` is in the constraint's domain.
   */
  final def approximation(param: TypeParamRef, fromBelow: Boolean): Type = {
    val avoidParam = new TypeMap {
      override def stopAtStatic = true
      def apply(tp: Type) = mapOver {
        tp match {
          case tp: RefinedType if param occursIn tp.refinedInfo => tp.parent
          case tp: WildcardType =>
            val bounds = tp.optBounds.orElse(TypeBounds.empty).bounds
            // Try to instantiate the wildcard to a type that is known to conform to it.
            // This means:
            //  If fromBelow is true, we minimize the type overall
            //  Hence, if variance < 0, pick the maximal safe type: bounds.lo
            //           (i.e. the whole bounds range is over the type)
            //         if variance > 0, pick the minimal safe type: bounds.hi
            //           (i.e. the whole bounds range is under the type)
            //         if variance == 0, pick bounds.lo anyway (this is arbitrary but in line with
            //           the principle that we pick the smaller type when in doubt).
            //  If fromBelow is false, we maximize the type overall and reverse the bounds
            //  if variance != 0. For variance == 0, we still minimize.
            //  In summary we pick the bound given by this table:
            //
            //  variance    | -1  0   1
            //  ------------------------
            //  from below  | lo  lo  hi
            //  from above  | hi  lo  lo
            //
            if (variance == 0 || fromBelow == (variance < 0)) bounds.lo else bounds.hi
          case _ => tp
        }
      }
    }
    assert(constraint.contains(param))
    val bound = if (fromBelow) constraint.fullLowerBound(param) else constraint.fullUpperBound(param)
    val inst = avoidParam(bound)
    typr.println(s"approx ${param.show}, from below = $fromBelow, bound = ${bound.show}, inst = ${inst.show}")
    inst
  }

  /** The instance type of `param` in the current constraint (which contains `param`).
   *  The instance type TI is computed depending on the given variance as follows:
   *
   *  If `variance < 0`, `TI` is the glb of `param`'s upper bounds.
   *  If `variance > 0`, `TI` is the lub of `param`'s lower bounds.
   *                      However, if `TI` would be a singleton type, and the upper bound is
   *                      not a singleton type, `TI` is widened to a non-singleton type.
   *  If `variance = 0`, let `Lo1` be the lower bound of `param` and let `Lo2` be the
   *                     |-dominator of `Lo1`. If `Lo2` is not more ugly than `Lo1`, we add the
   *                     additional constraint that `param` should be a supertype of `Lo2`.
   *                     We then proceed as if `variance > 0`.
   *
   *  The reason for tweaking the `variance = 0` case is that in this case we have to make
   *  a guess, since no possible instance type is better than the other. We apply the heuristic
   *  that usually an |-type is not what is intended. E.g. given two cases of types
   *  `Some[Int]` and `None`, we'd naturally want `Option[Int]` as the inferred type, not
   *  `Some[Int] | None`. If `variance > 0` it's OK to infer the smaller `|-type` since
   *  we can always widen later to the intended type. But in non-variant situations, we
   *  have to stick to the initial guess.
   *
   *  On the other hand, sometimes the |-dominator is _not_ what we want. For instance, taking
   *  run/hmap.scala as an example, we have a lower bound
   *
   *     HEntry[$param$5, V] | HEntry[String("name"), V]
   *
   *  Its |-dominator is
   *
   *     HEntry[_ >: $param$11 & String("cat") <: String, V]
   *
   *  If we apply the additional constraint we get compilation failures because while
   *  we can find implicits for the |-type above, we cannot find implicits for the |-dominator.
   *  There's no hard criterion what should be considered ugly or not. For now we
   *  pick the degree of undefinedness of type parameters, i.e. how many type
   *  parameters are instantiated with wildcard types. Maybe we have to refine this
   *  in the future.
   *
   *  The whole thing is clearly not nice, and I would love to have a better criterion.
   *  In principle we are grappling with the fundamental shortcoming that local type inference
   *  sometimes has to guess what was intended. The more refined our type lattice becomes,
   *  the harder it is to make a choice.
   */
  def instanceType(param: TypeParamRef, variance: Int): Type = {
    def upperBound = constraint.fullUpperBound(param)
    def isSingleton(tp: Type): Boolean = tp match {
      case tp: SingletonType => true
      case AndType(tp1, tp2) => isSingleton(tp1) | isSingleton(tp2)
      case OrType(tp1, tp2) => isSingleton(tp1) & isSingleton(tp2)
      case _ => false
    }
    def isFullyDefined(tp: Type): Boolean = tp match {
      case tp: TypeVar => tp.isInstantiated && isFullyDefined(tp.instanceOpt)
      case tp: TypeProxy => isFullyDefined(tp.underlying)
      case tp: AndOrType => isFullyDefined(tp.tp1) && isFullyDefined(tp.tp2)
      case _ => true
    }
    def isOrType(tp: Type): Boolean = tp.stripTypeVar.dealias match {
      case tp: OrType => true
      case tp: RefinedOrRecType => isOrType(tp.parent)
      case AndType(tp1, tp2) => isOrType(tp1) | isOrType(tp2)
      case WildcardType(bounds: TypeBounds) => isOrType(bounds.hi)
      case _ => false
    }

    // First, consider whether we need to constrain with |-dominator
    if (variance == 0) {
      val lo1 = ctx.typeComparer.bounds(param).lo
      val lo2 = ctx.orDominator(lo1)
      if (lo1 ne lo2) {
        val ugliness = new TypeAccumulator[Int] {
          def apply(x: Int, tp: Type) = tp match {
            case tp: TypeBounds if !tp.isAlias => apply(apply(x + 1, tp.lo), tp.hi)
            case tp: AndOrType => apply(x, tp.tp1) max apply(x, tp.tp2)
            case _ => foldOver(x, tp)
          }
        }
        if (ugliness(0, lo2) <= ugliness(0, lo1)) {
          constr.println(i"apply additional constr $lo2 <: $param, was $lo1")
          lo2 <:< param
        }
        else
          constr.println(i"""refrain from adding constrint.
                            |ugliness($lo1) = ${ugliness(0, lo1)}
                            |ugliness($lo2) = ${ugliness(0, lo2)}""")
      }
    }

    // Then, solve the constraint.
    val fromBelow = variance >= 0
    var inst = approximation(param, fromBelow)

    // Then, if instance is from below and is a singleton type, yet
    // upper bound is not a singleton type, widen the instance.
    if (fromBelow && isSingleton(inst) && !isSingleton(upperBound))
      inst = inst.widen

    // Then, simplify.
    inst = inst.simplified

    // Finally, if the instance is from below and is a fully-defined union type, yet upper bound
    // is not a union type, harmonize the union type.
    // TODO: See whether we can merge this with the special treatment of dependent params in
    // simplified.
    if (fromBelow && isOrType(inst) && isFullyDefined(inst) && !isOrType(upperBound)) {
      inst = ctx.harmonizeUnion(inst)
    }

    inst
  }

  /** Constraint `c1` subsumes constraint `c2`, if under `c2` as constraint we have
   *  for all poly params `p` defined in `c2` as `p >: L2 <: U2`:
   *
   *     c1 defines p with bounds p >: L1 <: U1, and
   *     L2 <: L1, and
   *     U1 <: U2
   *
   *  Both `c1` and `c2` are required to derive from constraint `pre`, possibly
   *  narrowing it with further bounds.
   */
  protected final def subsumes(c1: Constraint, c2: Constraint, pre: Constraint): Boolean =
    if (c2 eq pre) true
    else if (c1 eq pre) false
    else {
      val saved = constraint
      try
        c2.forallParams(p =>
          c1.contains(p) &&
          c2.upper(p).forall(c1.isLess(p, _)) &&
          isSubTypeWhenFrozen(c1.nonParamBounds(p), c2.nonParamBounds(p)))
      finally constraint = saved
    }

  /** The current bounds of type parameter `param` */
  final def bounds(param: TypeParamRef): TypeBounds = {
    val e = constraint.entry(param)
    if (e.exists) e.bounds else param.binder.paramInfos(param.paramNum)
  }

  /** Add type lambda `tl`, possibly with type variables `tvars`, to current constraint
   *  and propagate all bounds.
   *  @param tvars   See Constraint#add
   */
  def addToConstraint(tl: TypeLambda, tvars: List[TypeVar]): Unit =
    assert {
      checkPropagated(i"initialized $tl") {
        constraint = constraint.add(tl, tvars)
        tl.paramNames.indices.forall { i =>
          val param = TypeParamRef(tl, i)
          val bounds = constraint.nonParamBounds(param)
          val lower = constraint.lower(param)
          val upper = constraint.upper(param)
          if (lower.nonEmpty && !bounds.lo.isRef(defn.NothingClass) ||
            upper.nonEmpty && !bounds.hi.isRef(defn.AnyClass)) constr.println(i"INIT*** $tl")
          lower.forall(addOneBound(_, bounds.hi, isUpper = true)) &&
            upper.forall(addOneBound(_, bounds.lo, isUpper = false))
        }
      }
    }

  /** Can `param` be constrained with new bounds? */
  final def canConstrain(param: TypeParamRef): Boolean =
    !frozenConstraint && (constraint contains param)

  /** Add constraint `param <: bound` if `fromBelow` is false, `param >: bound` otherwise.
   *  `bound` is assumed to be in normalized form, as specified in `firstTry` and
   *  `secondTry` of `TypeComparer`. In particular, it should not be an alias type,
   *  lazy ref, typevar, wildcard type, error type. In addition, upper bounds may
   *  not be AndTypes and lower bounds may not be OrTypes. This is assured by the
   *  way isSubType is organized.
   */
  protected def addConstraint(param: TypeParamRef, bound: Type, fromBelow: Boolean): Boolean = {
    def description = i"constr $param ${if (fromBelow) ">:" else "<:"} $bound:\n$constraint"
    //checkPropagated(s"adding $description")(true) // DEBUG in case following fails
    checkPropagated(s"added $description") {
      addConstraintInvocations += 1

      /** When comparing lambdas we might get constraints such as
       *  `A <: X0` or `A = List[X0]` where `A` is a constrained parameter
       *  and `X0` is a lambda parameter. The constraint for `A` is not allowed
       *  to refer to such a lambda parameter because the lambda parameter is
       *  not visible where `A` is defined. Consequently, we need to
       *  approximate the bound so that the lambda parameter does not appear in it.
       *  If `tp` is an upper bound, we need to approximate with something smaller,
       *  otherwise something larger.
       *  Test case in pos/i94-nada.scala. This test crashes with an illegal instance
       *  error in Test2 when the rest of the SI-2712 fix is applied but `pruneLambdaParams` is
       *  missing.
       */
      def pruneLambdaParams(tp: Type) =
        if (comparedTypeLambdas.nonEmpty) {
          val approx = new ApproximatingTypeMap {
            def apply(t: Type): Type = t match {
              case t @ TypeParamRef(tl: TypeLambda, n) if comparedTypeLambdas contains tl =>
                val effectiveVariance = if (fromBelow) -variance else variance
                val bounds = tl.paramInfos(n)
                if (effectiveVariance > 0) bounds.lo
                else if (effectiveVariance < 0) bounds.hi
                else NoType
              case _ =>
                mapOver(t)
            }
          }
          approx(tp)
        }
        else tp

      def addParamBound(bound: TypeParamRef) =
        if (fromBelow) addLess(bound, param) else addLess(param, bound)

      /** Drop all constrained parameters that occur at the toplevel in `bound` and
       *  handle them by `addLess` calls.
       *  The preconditions make sure that such parameters occur only
       *  in one of two ways:
       *
       *  1.
       *
       *    P <: Ts1 | ... | Tsm   (m > 0)
       *    Tsi = T1 & ... Tn      (n >= 0)
       *    Some of the Ti are constrained parameters
       *
       *  2.
       *
       *    Ts1 & ... & Tsm <: P   (m > 0)
       *    Tsi = T1 | ... | Tn    (n >= 0)
       *    Some of the Ti are constrained parameters
       *
       *  In each case we cannot leave the parameter in place,
       *  because that would risk making a parameter later a subtype or supertype
       *  of a bound where the parameter occurs again at toplevel, which leads to cycles
       *  in the subtyping test. So we intentionally narrow the constraint by
       *  recording an isLess relationship instead (even though this is not implied
       *  by the bound).
       *
       *  Narrowing a constraint is better than widening it, because narrowing leads
       *  to incompleteness (which we face anyway, see for instance eitherIsSubType)
       *  but widening leads to unsoundness.
       *
       *  A test case that demonstrates the problem is i864.scala.
       *  Turn Config.checkConstraintsSeparated on to get an accurate diagnostic
       *  of the cycle when it is created.
       *
       *  @return The pruned type if all `addLess` calls succeed, `NoType` otherwise.
       */
      def prune(bound: Type): Type = bound match {
        case bound: AndOrType =>
          val p1 = prune(bound.tp1)
          val p2 = prune(bound.tp2)
          if (p1.exists && p2.exists) bound.derivedAndOrType(p1, p2)
          else NoType
        case bound: TypeVar if constraint contains bound.origin =>
          prune(bound.underlying)
        case bound: TypeParamRef =>
          constraint.entry(bound) match {
            case NoType => pruneLambdaParams(bound)
            case _: TypeBounds =>
              if (!addParamBound(bound)) NoType
              else if (fromBelow) defn.NothingType
              else defn.AnyType
            case inst =>
              prune(inst)
          }
        case _ =>
          pruneLambdaParams(bound)
      }

      try bound match {
        case bound: TypeParamRef if constraint contains bound =>
          addParamBound(bound)
        case _ =>
          val pbound = prune(bound)
          pbound.exists && (
            if (fromBelow) addLowerBound(param, pbound) else addUpperBound(param, pbound))
      }
      finally addConstraintInvocations -= 1
    }
  }

  /** Instantiate `param` to `tp` if the constraint stays satisfiable */
  protected def tryInstantiate(param: TypeParamRef, tp: Type): Boolean = {
    val saved = constraint
    constraint =
      if (addConstraint(param, tp, fromBelow = true) &&
          addConstraint(param, tp, fromBelow = false)) constraint.replace(param, tp)
      else saved
    constraint ne saved
  }

  /** Check that constraint is fully propagated. See comment in Config.checkConstraintsPropagated */
  def checkPropagated(msg: => String)(result: Boolean): Boolean = {
    if (Config.checkConstraintsPropagated && result && addConstraintInvocations == 0) {
      val saved = frozenConstraint
      frozenConstraint = true
      for (p <- constraint.domainParams) {
        def check(cond: => Boolean, q: TypeParamRef, ordering: String, explanation: String): Unit =
          assert(cond, i"propagation failure for $p $ordering $q: $explanation\n$msg")
        for (u <- constraint.upper(p))
          check(bounds(p).hi <:< bounds(u).hi, u, "<:", "upper bound not propagated")
        for (l <- constraint.lower(p)) {
          check(bounds(l).lo <:< bounds(p).hi, l, ">:", "lower bound not propagated")
          check(constraint.isLess(l, p), l, ">:", "reverse ordering (<:) missing")
        }
      }
      frozenConstraint = saved
    }
    result
  }
}
