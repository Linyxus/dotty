package dotty.tools
package dotc
package core

import Decorators._
import Contexts._
import Types._
import Symbols._
import util.SimpleIdentityMap
import collection.mutable
import printing._
import dotty.tools.dotc.reporting.trace

import Names.{Name, TypeName}

import scala.annotation.internal.sharable

/** Represents GADT constraints currently in scope */
sealed abstract class GadtConstraint extends Showable {
  /** Immediate bounds of `sym`. Does not contain lower/upper symbols (see [[fullBounds]]). */
  def bounds(sym: Symbol)(using Context): TypeBounds

  /** Full bounds of `sym`, including TypeRefs to other lower/upper symbols.
   *
   * @note this performs subtype checks between ordered symbols.
   *       Using this in isSubType can lead to infinite recursion. Consider `bounds` instead.
   */
  def fullBounds(sym: Symbol)(using Context): TypeBounds
  def simpleFullBounds(sym: Symbol)(using Context): TypeBounds
  def fullBoundsForName(name: Name)(using Context): TypeBounds

  /** Is `sym1` ordered to be less than `sym2`? */
  def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean

  /** Add symbols to constraint, correctly handling inter-dependencies.
   *
   * @see [[ConstraintHandling.addToConstraint]]
   */
  def addToConstraint(syms: List[Symbol])(using Context): Boolean
  def addToConstraint(sym: Symbol)(using Context): Boolean = addToConstraint(sym :: Nil)

  def addTypeMembersToConstraint(members: List[(Name, TypeBounds)])(using Context): Option[List[Name]]

  /** Further constrain a symbol already present in the constraint. */
  def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean

  def equalizeNames(name1: Name, name2: Name)(using Context): Boolean

  /** Is the symbol registered in the constraint?
   *
   * @note this is true even if the symbol is constrained to be equal to another type, unlike [[Constraint.contains]].
   */
  def contains(sym: Symbol)(using Context): Boolean

  def isEmpty: Boolean
  final def nonEmpty: Boolean = !isEmpty

  /** See [[ConstraintHandling.approximation]] */
  def approximation(sym: Symbol, fromBelow: Boolean)(using Context): Type

  def fresh: GadtConstraint

  /** Restore the state from other [[GadtConstraint]], probably copied using [[fresh]] */
  def restore(other: GadtConstraint): Unit

  def debugBoundsDescription(using Context): String

  /** Look into the constrainer to find the associated [[TypeVar]] for [[Symbol]] */
  def debugGetTypeVar(sym: Symbol): TypeVar

  def debugGetTypeVarForName(name: Name): TypeVar

  /** Record the name of the scrutinee being constrained upon */
  def setScrutName(scrut: String): Unit
  def scrutName: Option[String]

  /** Record type bounds from refinement types */
  def addScrutStructBound(name: Name, bound: Type): Unit
  def addPatStructBound(name: Name, bound: Type): Unit

  def scrutStructBounds: List[(Name, Type)]
  def patStructBounds: List[(Name, Type)]
}

final class ProperGadtConstraint private(
  private var myConstraint: Constraint,
  private var mapping: SimpleIdentityMap[Symbol, TypeVar],
  private var reverseMapping: SimpleIdentityMap[TypeParamRef, Symbol],
  private var nameMapping: SimpleIdentityMap[Name, TypeVar],
  private var reverseNameMapping: SimpleIdentityMap[TypeParamRef, Name],
  private var _scrutName: Option[String],
  private var myScrutStructBounds: List[(Name, Type)],
  private var myPatStructBounds: List[(Name, Type)],
) extends GadtConstraint with ConstraintHandling {
  import dotty.tools.dotc.config.Printers.{gadts, gadtsConstr}

  def this() = this(
    myConstraint = new OrderingConstraint(SimpleIdentityMap.empty, SimpleIdentityMap.empty, SimpleIdentityMap.empty),
    /** Mapping between symbols and internal types */
    mapping = SimpleIdentityMap.empty,
    reverseMapping = SimpleIdentityMap.empty,
    /** Mapping between names and internal types */
    nameMapping = SimpleIdentityMap.empty,
    reverseNameMapping = SimpleIdentityMap.empty,
    _scrutName = None,
    myScrutStructBounds = Nil,
    myPatStructBounds = Nil,
  )

  /** Exposes ConstraintHandling.subsumes */
  def subsumes(left: GadtConstraint, right: GadtConstraint, pre: GadtConstraint)(using Context): Boolean = {
    def extractConstraint(g: GadtConstraint) = g match {
      case s: ProperGadtConstraint => s.constraint
      case EmptyGadtConstraint => OrderingConstraint.empty
    }
    subsumes(extractConstraint(left), extractConstraint(right), extractConstraint(pre))
  }

  override def addTypeMembersToConstraint(members: List[(Name, TypeBounds)])(using Context): Option[List[Name]] = {
    import NameKinds.DepParamName

    val names = members map (_._1) map (x => DepParamName.fresh(x.toTypeName))

    def processBounds(pt: PolyType, tb: TypeBounds): TypeBounds = {
      def substDependentSyms(tp: Type, isUpper: Boolean)(using Context): Type = {
        def loop(tp: Type) = substDependentSyms(tp, isUpper)
        tp match {
          case tp @ AndType(tp1, tp2) =>
            tp.derivedAndType(loop(tp1), loop(tp2))
          case tp @ OrType(tp1, tp2) =>
            tp.derivedOrType(loop(tp1), loop(tp2))
          // special case for self-reference
          case TypeRef(RecThis(_), des : TypeName) =>
            val idx: Int = members map (_._1) indexOf des
            pt.paramRefs(idx.ensuring(_ != -1, i"should be a type member: $des"))
          case tp: NamedType =>
            mapping(tp.symbol) match {
              // type parameters
              case tv: TypeVar => tv.origin
              case null => tp
            }
          case tp => tp
        }
      }

      tb match {
        case alias : TypeAlias =>
          alias.derivedAlias(substDependentSyms(alias.lo, isUpper = false))
        case _ =>
          tb.derivedTypeBounds(
            lo = substDependentSyms(tb.lo, isUpper = false),
            hi = substDependentSyms(tb.hi, isUpper = true)
          )
      }
    }

    val poly1 = PolyType(names)(
      pt => members map { case (name, bounds) =>
        val bounds2 = processBounds(pt, bounds)
        // TypeBounds(defn.NothingType, defn.AnyType)
        gadts.println(i"processed bounds of $name : $bounds2")
        bounds2
      },
      pt => defn.AnyType
    )

    val tvars = names.lazyZip(poly1.paramRefs).map { case (name, paramRef) =>
      val tv = TypeVar(paramRef, creatorState = null)
      nameMapping = nameMapping.updated(name, tv)
      reverseNameMapping = reverseNameMapping.updated(tv.origin, name)
      tv
    }

    // The replaced symbols are picked up here.
    val res = {
      addToConstraint(poly1, tvars)
        .showing(i"added to constraint: [$poly1] $members%, %\n$debugBoundsDescription", gadts)
    }

    if res then Some(names) else None
  }

  override def addToConstraint(params: List[Symbol])(using Context): Boolean = {
    import NameKinds.DepParamName

    val poly1 = PolyType(params.map { sym => DepParamName.fresh(sym.name.toTypeName) })(
      pt => params.map { param =>
        // In bound type `tp`, replace the symbols in dependent positions with their internal TypeParamRefs.
        // The replaced symbols will be later picked up in `ConstraintHandling#addToConstraint`
        // and used as orderings.
        def substDependentSyms(tp: Type, isUpper: Boolean)(using Context): Type = {
          def loop(tp: Type) = substDependentSyms(tp, isUpper)
          tp match {
            case tp @ AndType(tp1, tp2) if !isUpper =>
              tp.derivedAndType(loop(tp1), loop(tp2))
            case tp @ OrType(tp1, tp2) if isUpper =>
              tp.derivedOrType(loop(tp1), loop(tp2))
            case tp: NamedType =>
              params.indexOf(tp.symbol) match {
                case -1 =>
                  mapping(tp.symbol) match {
                    case tv: TypeVar => tv.origin
                    case null => tp
                  }
                case i => pt.paramRefs(i)
              }
            case tp => tp
          }
        }

        val tb = param.info.bounds
        tb.derivedTypeBounds(
          lo = substDependentSyms(tb.lo, isUpper = false),
          hi = substDependentSyms(tb.hi, isUpper = true)
        )
      },
      pt => defn.AnyType
    )

    val tvars = params.lazyZip(poly1.paramRefs).map { (sym, paramRef) =>
      val tv = TypeVar(paramRef, creatorState = null)
      mapping = mapping.updated(sym, tv)
      reverseMapping = reverseMapping.updated(tv.origin, sym)
      tv
    }

    // The replaced symbols are picked up here.
    addToConstraint(poly1, tvars)
      .showing(i"added to constraint: [$poly1] $params%, %\n$debugBoundsDescription", gadts)
  }

  private def _addBound[T](m: T => TypeVar)(ident: T, bound: Type, isUpper: Boolean)(using Context): Boolean = {
    @annotation.tailrec def stripInternalTypeVar(tp: Type): Type = tp match {
      case tv: TypeVar =>
        val inst = constraint.instType(tv)
        if (inst.exists) stripInternalTypeVar(inst) else tv
      case _ => tp
    }

    val symTvar: TypeVar = stripInternalTypeVar(tvarOrError(m)(ident)) match {
      case tv: TypeVar => tv
      case inst =>
        gadts.println(i"instantiated: $ident -> $inst")
        return if (isUpper) isSub(inst, bound) else isSub(bound, inst)
    }

    val internalizedBound = bound match {
      case nt: NamedType =>
        val ntTvar = mapping(nt.symbol)
        if (ntTvar ne null) stripInternalTypeVar(ntTvar) else bound
      case _ => bound
    }
    (
      internalizedBound match {
        case boundTvar: TypeVar =>
          if (boundTvar eq symTvar) true
          else if (isUpper) addLess(symTvar.origin, boundTvar.origin)
          else addLess(boundTvar.origin, symTvar.origin)
        case bound =>
          addBoundTransitively(symTvar.origin, bound, isUpper)
      }
    ).showing({
      val descr = if (isUpper) "upper" else "lower"
      val op = if (isUpper) "<:" else ">:"
      i"adding $descr bound $ident $op $bound = $result"
    }, gadts)
  }

  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = trace(i"addBound($sym, $bound, ${if isUpper then "upper" else "lower"})", gadts) { _addBound(mapping)(sym, bound, isUpper) }

  private def addBoundForName(name: Name, bound: Type, isUpper: Boolean)(using Context): Boolean = _addBound(nameMapping)(name, bound, isUpper)

  override def equalizeNames(name1: Name, name2: Name)(using Context): Boolean = {
    @annotation.tailrec def stripInternalTypeVar(tp: Type): Type = tp match {
      case tv: TypeVar =>
        val inst = constraint.instType(tv)
        if (inst.exists) stripInternalTypeVar(inst) else tv
      case _ => tp
    }

    val tvar1 = tvarOrError(nameMapping)(name1)
    val tvar2 = tvarOrError(nameMapping)(name2)

    val stv1 = stripInternalTypeVar(tvar1)
    val stv2 = stripInternalTypeVar(tvar2)

    (stripInternalTypeVar(stv1), stripInternalTypeVar(stv2)) match {
      case (stv1 : TypeVar, stv2 : TypeVar) =>
        addBoundForName(name1, stv2, isUpper = false) && addBoundForName(name2, stv1, isUpper = false)
      case (inst1, _ : TypeVar) =>
        addBoundForName(name2, inst1, isUpper = false) && addBoundForName(name2, inst1, isUpper = true)
      case (_ : TypeVar, inst2) =>
        addBoundForName(name1, inst2, isUpper = false) && addBoundForName(name1, inst2, isUpper = true)
      case (inst1, inst2) =>
        isSame(inst1, inst2)
    }
  }


  override def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean = trace(i"isLess($sym1, $sym2)", gadtsConstr) {
    constraint.isLess(tvarOrError(mapping)(sym1).origin, tvarOrError(mapping)(sym2).origin)
  }

  override def fullBounds(sym: Symbol)(using Context): TypeBounds =
    mapping(sym) match {
      case null => null
      case tv =>
        fullBounds(tv.origin)
          // .ensuring(containsNoInternalTypes(_))
    }

  override def simpleFullBounds(sym: Symbol)(using Context): TypeBounds =
    mapping(sym) match {
      case null => null
      case tv =>
        simpleFullBounds(tv.origin)
        // .ensuring(containsNoInternalTypes(_))
    }

  override def fullBoundsForName(name: Name)(using Context): TypeBounds =
    nameMapping(name) match {
      case null => null
      case tv =>
        fullBounds(tv.origin)
    }

  override def bounds(sym: Symbol)(using Context): TypeBounds = trace(i"bounds($sym)", gadts, show = true) {
    mapping(sym) match {
      case null => null
      case tv =>
        def retrieveBounds: TypeBounds =
          bounds(tv.origin) match {
            case TypeAlias(tpr: TypeParamRef) if reverseMapping.contains(tpr) =>
              TypeAlias(reverseMapping(tpr).typeRef)
            case tb => tb
          }
        retrieveBounds
        //.showing(i"gadt bounds $sym: $result", gadts)
        //.ensuring(containsNoInternalTypes(_))
    }
  }

  override def contains(sym: Symbol)(using Context): Boolean = trace(s"contrains(${sym.denot.typeRef})", gadts) { mapping(sym) ne null }

  override def approximation(sym: Symbol, fromBelow: Boolean)(using Context): Type = {
    val res = approximation(tvarOrError(mapping)(sym).origin, fromBelow = fromBelow)
    gadts.println(i"approximating $sym ~> $res")
    res
  }

  override def fresh: GadtConstraint = new ProperGadtConstraint(
    myConstraint,
    mapping,
    reverseMapping,
    nameMapping,
    reverseNameMapping,
    None,
    Nil,
    Nil,
  )

  def restore(other: GadtConstraint): Unit = other match {
    case other: ProperGadtConstraint =>
      this.myConstraint = other.myConstraint
      this.mapping = other.mapping
      this.reverseMapping = other.reverseMapping
      this.nameMapping = other.nameMapping
      this.reverseNameMapping = other.reverseNameMapping
    case _ => ;
  }

  override def isEmpty: Boolean = mapping.size == 0

  // ---- Protected/internal -----------------------------------------------

  override protected def constraint = myConstraint
  override protected def constraint_=(c: Constraint) = myConstraint = c

  override protected def isSub(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSubType(tp1, tp2)
  override protected def isSame(tp1: Type, tp2: Type)(using Context): Boolean = TypeComparer.isSameType(tp1, tp2)

  override def nonParamBounds(param: TypeParamRef)(using Context): TypeBounds =
    val externalizeMap = new TypeMap {
      def apply(tp: Type): Type = tp match {
        case tpr: TypeParamRef => externalize(tpr)
        case tp => mapOver(tp)
      }
    }
    externalizeMap(constraint.nonParamBounds(param)).bounds

  override def fullLowerBound(param: TypeParamRef)(using Context): Type =
    constraint.minLower(param).foldLeft(nonParamBounds(param).lo) {
      (t, u) => t | externalize(u)
    }

  override def fullUpperBound(param: TypeParamRef)(using Context): Type = trace.force("fullUpperBound inner", gadts, show = true) {
    trace.force(i"constraint.minUpper of $param", gadts, show = true) { constraint.minUpper(param) }.foldLeft(trace.force(i"nonParamBounds.hi of $param", gadts, show = true) { nonParamBounds(param).hi }) { (t, u) =>
      val eu = externalize(u)
      // Any as the upper bound means "no bound", but if F is higher-kinded,
      // Any & F = F[_]; this is wrong for us so we need to short-circuit
      if t.isAny then eu else t & eu
    }
  }

  override def simpleFullLowerBound(param: TypeParamRef)(using Context): Type =
    constraint.minLower(param).foldLeft(nonParamBounds(param).lo) { (t, u) =>
      OrType(t, externalize(u), soft = false)
    }

  override def simpleFullUpperBound(param: TypeParamRef)(using Context): Type =
    constraint.minUpper(param).foldLeft(nonParamBounds(param).hi) { (t, u) =>
      val eu = externalize(u)

      if t.isAny then eu else AndType(t, eu)
    }

  // ---- Private ----------------------------------------------------------

  private def externalize(param: TypeParamRef)(using Context): Type =
    reverseMapping(param) match {
      case sym: Symbol => sym.typeRef
      case null => param
    }

  private def tvarOrError[T](mapping: T => TypeVar)(ident: T)(using Context): TypeVar =
    mapping(ident).ensuring(_ ne null, i"not a constrainable symbol: $ident")

  private def containsNoInternalTypes(
    tp: Type,
    acc: TypeAccumulator[Boolean] = null
  )(using Context): Boolean = tp match {
    case tpr: TypeParamRef => !reverseMapping.contains(tpr)
    case tv: TypeVar => !reverseMapping.contains(tv.origin)
    case tp =>
      (if (acc ne null) acc else new ContainsNoInternalTypesAccumulator()).foldOver(true, tp)
  }

  private class ContainsNoInternalTypesAccumulator(using Context) extends TypeAccumulator[Boolean] {
    override def apply(x: Boolean, tp: Type): Boolean = x && containsNoInternalTypes(tp)
  }

  // ---- Debug ------------------------------------------------------------

  override def constr = gadtsConstr

  override def toText(printer: Printer): Texts.Text = constraint.toText(printer)

  override def debugBoundsDescription(using Context): String = {
    val sb = new mutable.StringBuilder
    sb ++= constraint.show
    sb += '\n'
    mapping.foreachBinding { case (sym, _) =>
      sb ++= i"$sym: ${fullBounds(sym)}\n"
    }
    nameMapping.foreachBinding { case (name, _) =>
      sb ++= i"$name: ${fullBoundsForName(name)}\n"
    }
    sb.result
  }

  override def debugGetTypeVar(sym: Symbol): TypeVar = mapping(sym)

  override def debugGetTypeVarForName(name: Name): TypeVar = nameMapping(name)

  override def setScrutName(scrut: String): Unit = { _scrutName = Some(scrut) }
  override def scrutName: Option[String] = _scrutName

  override def addScrutStructBound(name: Name, bound: Type): Unit =
    myScrutStructBounds = (name, bound) :: myScrutStructBounds
  override def addPatStructBound(name: Name, bound: Type): Unit =
    myPatStructBounds = (name, bound) :: myPatStructBounds

  override def scrutStructBounds: List[(Name, Type)] = myScrutStructBounds
  override def patStructBounds: List[(Name, Type)] = myPatStructBounds
}

@sharable object EmptyGadtConstraint extends GadtConstraint {
  override def bounds(sym: Symbol)(using Context): TypeBounds = null
  override def fullBounds(sym: Symbol)(using Context): TypeBounds = null
  override def simpleFullBounds(sym: Symbol)(using Context): TypeBounds = null
  override def fullBoundsForName(name: Name)(using Context): TypeBounds = null

  override def isLess(sym1: Symbol, sym2: Symbol)(using Context): Boolean = unsupported("EmptyGadtConstraint.isLess")

  override def isEmpty: Boolean = true

  override def contains(sym: Symbol)(using Context) = false

  override def addToConstraint(params: List[Symbol])(using Context): Boolean = unsupported("EmptyGadtConstraint.addToConstraint")
  override def addBound(sym: Symbol, bound: Type, isUpper: Boolean)(using Context): Boolean = unsupported("EmptyGadtConstraint.addBound")

  override def equalizeNames(name1: Name, name2: Name)(using Context): Boolean = unsupported("EmptyGadtConstraint.equalizeNames")

  override def addTypeMembersToConstraint(members: List[(Name, TypeBounds)])(using Context): Option[List[Name]] = unsupported("EmptyGadtConstraint.addTypeMemebrsToCohnstraints")

  override def approximation(sym: Symbol, fromBelow: Boolean)(using Context): Type = unsupported("EmptyGadtConstraint.approximation")

  override def fresh = new ProperGadtConstraint
  override def restore(other: GadtConstraint): Unit =
    if (!other.isEmpty) sys.error("cannot restore a non-empty GADTMap")

  override def debugBoundsDescription(using Context): String = "EmptyGadtConstraint"

  override def toText(printer: Printer): Texts.Text = "EmptyGadtConstraint"

  override def setScrutName(scrut: String): Unit = ()
  override def scrutName: Option[String] = None

  override def addScrutStructBound(name: Name, bound: Type): Unit = ()
  override def addPatStructBound(name: Name, bound: Type): Unit = ()

  override def scrutStructBounds: List[(Name, Type)] = Nil
  override def patStructBounds: List[(Name, Type)] = Nil

  override def debugGetTypeVar(sym: Symbol): TypeVar = unsupported("EmptyGadtConstraint.debugGetTypeVar")
  override def debugGetTypeVarForName(name: Name): TypeVar = unsupported("EmptyGadtConstraint.debugGetTypeVarForName")
}
