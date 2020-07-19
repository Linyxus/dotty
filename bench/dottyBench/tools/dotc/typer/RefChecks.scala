package dottyBench.tools.dotc
package typer

import transform._
import core._
import Symbols._, Types._, Contexts._, Flags._, Names._, NameOps._
import StdNames._, Denotations._, SymUtils._, Phases._
import NameKinds.DefaultGetterName
import Annotations._
import util.Spans._
import util.{Store, SourcePosition}
import scala.collection.{ mutable, immutable }
import ast._
import MegaPhase._
import config.Printers.{checks, noPrinter}
import scala.util.{Try, Failure, Success}
import config.{ScalaVersion, NoScalaVersion}
import Decorators._
import typer.ErrorReporting._
import config.Feature.warnOnMigration
import reporting._

object RefChecks {
  import tpd.{Tree, MemberDef}

  val name: String = "refchecks"

  private val defaultMethodFilter = new NameFilter {
    def apply(pre: Type, name: Name)(using Ctx, CState): Boolean = name.is(DefaultGetterName)
  }

  /** Only one overloaded alternative is allowed to define default arguments */
  private def checkOverloadedRestrictions(clazz: Symbol)(using Ctx, CState): Unit = {
    // Using the default getters (such as methodName$default$1) as a cheap way of
    // finding methods with default parameters. This way, we can limit the members to
    // those with the DEFAULTPARAM flag, and infer the methods. Looking for the methods
    // directly requires inspecting the parameter list of every one. That modification
    // shaved 95% off the time spent in this method.

    for {
      defaultGetterClass <- List(clazz, clazz.companionModule.moduleClass);
      if defaultGetterClass.isClass
    }
    {
      val defaultGetterNames = defaultGetterClass.asClass.memberNames(defaultMethodFilter)
      val defaultMethodNames = defaultGetterNames map { _ replace {
        case DefaultGetterName(methName, _) => methName
      }}

      for (name <- defaultMethodNames) {
        val methods = clazz.info.member(name).alternatives.map(_.symbol)
        val haveDefaults = methods.filter(_.hasDefaultParams)
        if (haveDefaults.length > 1) {
          val owners = haveDefaults map (_.owner)
          // constructors of different classes are allowed to have defaults
          if (haveDefaults.exists(x => !x.isConstructor) || owners.distinct.size < haveDefaults.size)
            report.error(
              "in " + clazz +
                ", multiple overloaded alternatives of " + haveDefaults.head +
                " define default arguments" + (
                  if (owners.forall(_ == clazz)) "."
                  else ".\nThe members with defaults are defined in " + owners.map(_.showLocated).mkString("", " and ", ".")),
              clazz.sourcePos)
        }
      }
    }

    // Check for doomed attempt to overload applyDynamic
    if (clazz derivesFrom defn.DynamicClass)
      for ((_, m1 :: m2 :: _) <- (clazz.info member nme.applyDynamic).alternatives groupBy (_.symbol.typeParams.length))
        report.error("implementation restriction: applyDynamic cannot be overloaded except by methods with different numbers of type parameters, e.g. applyDynamic[T1](method: String)(arg: T1) and applyDynamic[T1, T2](method: String)(arg1: T1, arg2: T2)",
          m1.symbol.sourcePos)
  }

  /** The this-type of `cls` which should be used when looking at the types of
   *  inherited members. If `cls` has a non-trivial self type, this returns a skolem
   *  with the class type instead of the `this-type` of the class as usual.
   *  This is done because otherwise we want to understand inherited infos
   *  as they are written, whereas with the this-type they could be
   *  more special. A test where this makes a difference is pos/i1401.scala.
   *  This one used to succeed only if forwarding parameters is on.
   *  (Forwarding tends to hide problems by binding parameter names).
   */
  private def upwardsThisType(cls: Symbol)(using Ctx, CState) = cls.info match {
    case ClassInfo(_, _, _, _, tp: Type) if (tp ne cls.typeRef) && !cls.isOneOf(FinalOrModuleClass) =>
      SkolemType(cls.appliedRef).withName(nme.this_)
    case _ =>
      cls.thisType
  }

  /** Check that self type of this class conforms to self types of parents.
   *  and required classes.
   */
  private def checkParents(cls: Symbol)(using Ctx, CState): Unit = cls.info match {
    case cinfo: ClassInfo =>
      def checkSelfConforms(other: ClassSymbol, category: String, relation: String) = {
        val otherSelf = other.declaredSelfTypeAsSeenFrom(cls.thisType)
        if otherSelf.exists && !(cinfo.selfType <:< otherSelf) then
          report.error(DoesNotConformToSelfType(category, cinfo.selfType, cls, otherSelf, relation, other.classSymbol),
            cls.sourcePos)
      }
      for (parent <- cinfo.classParents)
        checkSelfConforms(parent.classSymbol.asClass, "illegal inheritance", "parent")
      for (reqd <- cinfo.cls.givenSelfType.classSymbols)
        checkSelfConforms(reqd, "missing requirement", "required")
    case _ =>
  }

  /** Disallow using trait parameters as prefix for its parents.
   *
   *  The rationale is to ensure outer-related NPE never happen in Scala.
   *  Otherwise, outer NPE may happen, see tests/neg/i5083.scala
   */
  private def checkParentPrefix(cls: Symbol, parent: Tree)(using Ctx, CState): Unit =
    parent.tpe.typeConstructor match {
      case TypeRef(ref: TermRef, _) =>
        val paramRefs = ref.namedPartsWith(ntp => ntp.symbol.enclosingClass == cls)
        if (paramRefs.nonEmpty)
          report.error(TraitParameterUsedAsParentPrefix(cls), parent.sourcePos)
      case _ =>
    }

  /** Check that a class and its companion object to not both define
   *  a class or module with same name
   */
  private def checkCompanionNameClashes(cls: Symbol)(using Ctx, CState): Unit =
    if (!cls.owner.is(ModuleClass)) {
      def clashes(sym: Symbol) =
        sym.isClass &&
        sym.name.stripModuleClassSuffix == cls.name.stripModuleClassSuffix

      val others = cls.owner.linkedClass.info.decls.filter(clashes)
      others.foreach { other =>
        report.error(ClassAndCompanionNameClash(cls, other), cls.sourcePos)
      }
    }

  // Override checking ------------------------------------------------------------

  /** 1. Check all members of class `clazz` for overriding conditions.
   *  That is for overriding member M and overridden member O:
   *
   *    1.1. M must have the same or stronger access privileges as O.
   *    1.2. O must not be effectively final.
   *    1.3. If M or O are extension methods, they must both be extension methods.
   *         (Should be before the check for the need of `override` modifier.)
   *    1.4. O is deferred, or M has `override` modifier.
   *    1.5. If O is stable, then so is M.
   *    1.6. If O is a type alias, then M is an alias of O.
   *    1.7. If O is an abstract type then
   *       1.7.1 either M is an abstract type, and M's bounds are sharper than O's bounds.
   *             or M is a type alias or class which conforms to O's bounds.
   *       1.7.2 higher-order type arguments must respect bounds on higher-order type parameters  -- @M
   *              (explicit bounds and those implied by variance annotations) -- @see checkKindBounds
   *    1.8. If O and M are values, then
   *    1.8.1  M's type is a subtype of O's type, or
   *    1.8.2  M is of type []S, O is of type ()T and S <: T, or
   *    1.8.3  M is of type ()S, O is of type []T and S <: T, or
   *    1.9. If M is erased, O is erased. If O is erased, M is erased or inline.
   *    1.10.  If O is inline (and deferred, otherwise O would be final), M must be inline
   *    1.11.  If O is a Scala-2 macro, M must be a Scala-2 macro.
   *  2. Check that only abstract classes have deferred members
   *  3. Check that concrete classes do not have deferred definitions
   *     that are not implemented in a subclass.
   *  4. Check that every member with an `override` modifier
   *     overrides some other member.
   *  TODO check that classes are not overridden
   *  TODO This still needs to be cleaned up; the current version is a straight port of what was there
   *       before, but it looks too complicated and method bodies are far too large.
   */
  private def checkAllOverrides(clazz: ClassSymbol)(using Ctx, CState): Unit = {
    val self = clazz.thisType
    val upwardsSelf = upwardsThisType(clazz)
    var hasErrors = false

    case class MixinOverrideError(member: Symbol, msg: String)

    val mixinOverrideErrors = new mutable.ListBuffer[MixinOverrideError]()

    def printMixinOverrideErrors(): Unit =
      mixinOverrideErrors.toList match {
        case Nil =>
        case List(MixinOverrideError(_, msg)) =>
          report.error(msg, clazz.sourcePos)
        case MixinOverrideError(member, msg) :: others =>
          val others1 = others.map(_.member).filter(_.name != member.name).distinct
          def othersMsg = {
            val others1 = others.map(_.member)
              .filter(_.name != member.name)
              .map(_.show).distinct
            if (others1.isEmpty) ""
            else i";\nother members with override errors are:: $others1%, %"
          }
          report.error(msg + othersMsg, clazz.sourcePos)
      }

    def infoString(sym: Symbol) = infoString0(sym, sym.owner != clazz)
    def infoStringWithLocation(sym: Symbol) = infoString0(sym, true)

    def infoString0(sym: Symbol, showLocation: Boolean) = {
      val sym1 = sym.underlyingSymbol
      def info = self.memberInfo(sym1)
      val infoStr =
        if (sym1.isAliasType) i", which equals ${info.bounds.hi}"
        else if (sym1.isAbstractOrParamType) i" with bounds$info"
        else if (sym1.is(Module)) ""
        else if (sym1.isTerm) i" of type $info"
        else ""
      i"${if (showLocation) sym1.showLocated else sym1}$infoStr"
    }

    /* Check that all conditions for overriding `other` by `member`
       * of class `clazz` are met.
       */
    def checkOverride(member: Symbol, other: Symbol): Unit = {
      def memberTp(self: Type) =
        if (member.isClass) TypeAlias(member.typeRef.EtaExpand(member.typeParams))
        else self.memberInfo(member)
      def otherTp(self: Type) = self.memberInfo(other)

      report.debuglog("Checking validity of %s overriding %s".format(member.showLocated, other.showLocated))

      def noErrorType = !memberTp(self).isErroneous && !otherTp(self).isErroneous

      def overrideErrorMsg(msg: String): String = {
        val isConcreteOverAbstract =
          (other.owner isSubClass member.owner) && other.is(Deferred) && !member.is(Deferred)
        val addendum =
          if (isConcreteOverAbstract)
            ";\n  (Note that %s is abstract,\n  and is therefore overridden by concrete %s)".format(
              infoStringWithLocation(other),
              infoStringWithLocation(member))
          else if (ctx.settings.Ydebug.value)
            TypeMismatch(memberTp(self), otherTp(self))
          else ""

        "error overriding %s;\n  %s %s%s".format(
          infoStringWithLocation(other), infoString(member), msg, addendum)
      }

      def emitOverrideError(fullmsg: String) =
        if (!(hasErrors && member.is(Synthetic) && member.is(Module))) {
          // suppress errors relating toi synthetic companion objects if other override
          // errors (e.g. relating to the companion class) have already been reported.
          if (member.owner == clazz) report.error(fullmsg, member.sourcePos)
          else mixinOverrideErrors += new MixinOverrideError(member, fullmsg)
          hasErrors = true
        }

      def overrideError(msg: String) =
        if (noErrorType)
          emitOverrideError(overrideErrorMsg(msg))

      def autoOverride(sym: Symbol) =
        sym.is(Synthetic) && (
          desugar.isDesugaredCaseClassMethodName(member.name) || // such names are added automatically, can't have an override preset.
          sym.is(Module)) // synthetic companion

      def overrideAccessError() = {
        report.log(i"member: ${member.showLocated} ${member.flagsString}") // DEBUG
        report.log(i"other: ${other.showLocated} ${other.flagsString}") // DEBUG
        val otherAccess = (other.flags & AccessFlags).flagsString
        overrideError("has weaker access privileges; it should be " +
          (if (otherAccess == "") "public" else "at least " + otherAccess))
      }

      def compatibleTypes(memberTp: Type, otherTp: Type): Boolean =
        try
          if (member.isType) // intersection of bounds to refined types must be nonempty
            memberTp.bounds.hi.hasSameKindAs(otherTp.bounds.hi) &&
            ((memberTp frozen_<:< otherTp) ||
             !member.owner.derivesFrom(other.owner) && {
               // if member and other come from independent classes or traits, their
               // bounds must have non-empty-intersection
               val jointBounds = (memberTp.bounds & otherTp.bounds).bounds
               jointBounds.lo frozen_<:< jointBounds.hi
             })
          else
            member.name.is(DefaultGetterName) || // default getters are not checked for compatibility
            memberTp.overrides(otherTp,
                member.matchNullaryLoosely || other.matchNullaryLoosely ||
                warnOnMigration(overrideErrorMsg("no longer has compatible type"),
                   (if (member.owner == clazz) member else clazz).sourcePos))
        catch {
          case ex: MissingType =>
            // can happen when called with upwardsSelf as qualifier of memberTp and otherTp,
            // because in that case we might access types that are not members of the qualifier.
            false
        }

      //Console.println(infoString(member) + " overrides " + infoString(other) + " in " + clazz);//DEBUG

      // return if we already checked this combination elsewhere
      if (member.owner != clazz) {
        def deferredCheck = member.is(Deferred) || !other.is(Deferred)
        def subOther(s: Symbol) = s derivesFrom other.owner
        def subMember(s: Symbol) = s derivesFrom member.owner

        if (subOther(member.owner) && deferredCheck)
          //Console.println(infoString(member) + " shadows1 " + infoString(other) " in " + clazz);//DEBUG
          return
        val parentSymbols = clazz.info.parents.map(_.typeSymbol)
        if (parentSymbols exists (p => subOther(p) && subMember(p) && deferredCheck))
          //Console.println(infoString(member) + " shadows2 " + infoString(other) + " in " + clazz);//DEBUG
          return
        if (parentSymbols forall (p => subOther(p) == subMember(p)))
          //Console.println(infoString(member) + " shadows " + infoString(other) + " in " + clazz);//DEBUG
          return
      }

      /* Is the intersection between given two lists of overridden symbols empty? */
      def intersectionIsEmpty(syms1: Iterator[Symbol], syms2: Iterator[Symbol]) = {
        val set2 = syms2.toSet
        !(syms1 exists (set2 contains _))
      }

      // o: public | protected        | package-protected  (aka java's default access)
      // ^-may be overridden by member with access privileges-v
      // m: public | public/protected | public/protected/package-protected-in-same-package-as-o

      if (member.is(Private)) // (1.1)
        overrideError("has weaker access privileges; it should not be private")

      // todo: align accessibility implication checking with isAccessible in Contexts
      val ob = other.accessBoundary(member.owner)
      val mb = member.accessBoundary(member.owner)
      def isOverrideAccessOK =
           (member.flags & AccessFlags).isEmpty
           && !member.privateWithin.exists // member is public, or
        || (!other.is(Protected) || member.is(Protected))
              // if o is protected, so is m, and
           && (ob.isContainedIn(mb) || other.isAllOf(JavaProtected))
             // m relaxes o's access boundary,
             // or o is Java defined and protected (see #3946)
      if (!isOverrideAccessOK)
        overrideAccessError()
      else if (other.isClass)
        // direct overrides were already checked on completion (see Checking.chckWellFormed)
        // the test here catches indirect overriddes between two inherited base types.
        overrideError("cannot be used here - class definitions cannot be overridden")
      else if (!other.is(Deferred) && member.isClass)
        overrideError("cannot be used here - classes can only override abstract types")
      else if (other.isEffectivelyFinal) // (1.2)
        overrideError(i"cannot override final member ${other.showLocated}")
      else if (member.isAllOf(ExtensionMethod) && !other.isAllOf(ExtensionMethod)) // (1.3)
        overrideError("is an extension method, cannot override a normal method")
      else if (other.isAllOf(ExtensionMethod) && !member.isAllOf(ExtensionMethod)) // (1.3)
        overrideError("is a normal method, cannot override an extension method")
      else if (!other.is(Deferred) &&
                 !other.name.is(DefaultGetterName) &&
                 !member.isAnyOverride)
        // Exclusion for default getters, fixes SI-5178. We cannot assign the Override flag to
        // the default getter: one default getter might sometimes override, sometimes not. Example in comment on ticket.
        // Also exclusion for implicit shortcut methods
        // Also excluded under Scala2 mode are overrides of default methods of Java traits.
        if (autoOverride(member) ||
            other.owner.isAllOf(JavaInterface) &&
            warnOnMigration("`override` modifier required when a Java 8 default method is re-implemented", member.sourcePos))
          member.setFlag(Override)
        else if (member.isType && self.memberInfo(member) =:= self.memberInfo(other))
          () // OK, don't complain about type aliases which are equal
        else if (member.owner != clazz && other.owner != clazz &&
                 !(other.owner derivesFrom member.owner))
          emitOverrideError(
            s"$clazz inherits conflicting members:\n  "
              + infoStringWithLocation(other) + "  and\n  " + infoStringWithLocation(member)
              + "\n(Note: this can be resolved by declaring an override in " + clazz + ".)")
        else if member.is(Exported) then
          overrideError("cannot override since it comes from an export")
        else
          overrideError("needs `override` modifier")
      else if (other.is(AbsOverride) && other.isIncompleteIn(clazz) && !member.is(AbsOverride))
        overrideError("needs `abstract override` modifiers")
      else if (member.is(Override) && other.is(Accessor) &&
        other.accessedFieldOrGetter.is(Mutable, butNot = Lazy)) {
        // !?! this is not covered by the spec. We need to resolve this either by changing the spec or removing the test here.
        // !!! is there a !?! convention? I'm !!!ing this to make sure it turns up on my searches.
        if (!ctx.settings.YoverrideVars.value)
          overrideError("cannot override a mutable variable")
      }
      else if (member.isAnyOverride &&
        !(member.owner.thisType.baseClasses exists (_ isSubClass other.owner)) &&
        !member.is(Deferred) && !other.is(Deferred) &&
        intersectionIsEmpty(member.extendedOverriddenSymbols, other.extendedOverriddenSymbols))
        overrideError("cannot override a concrete member without a third member that's overridden by both " +
          "(this rule is designed to prevent ``accidental overrides'')")
      else if (other.isStableMember && !member.isStableMember) // (1.5)
        overrideError("needs to be a stable, immutable value")
      else if (member.is(ModuleVal) && !other.isRealMethod && !other.isOneOf(Deferred | Lazy))
        overrideError("may not override a concrete non-lazy value")
      else if (member.is(Lazy, butNot = Module) && !other.isRealMethod && !other.is(Lazy) &&
                 !warnOnMigration(overrideErrorMsg("may not override a non-lazy value"), member.sourcePos))
        overrideError("may not override a non-lazy value")
      else if (other.is(Lazy) && !other.isRealMethod && !member.is(Lazy))
        overrideError("must be declared lazy to override a lazy value")
      else if (member.is(Erased) && !other.is(Erased)) // (1.9)
        overrideError("is erased, cannot override non-erased member")
      else if (other.is(Erased) && !member.isOneOf(Erased | Inline)) // (1.9)
        overrideError("is not erased, cannot override erased member")
      else if other.is(Inline) && !member.is(Inline) then // (1.10)
        overrideError("is not inline, cannot implement an inline method")
      else if (other.isScala2Macro && !member.isScala2Macro) // (1.11)
        overrideError("cannot be used here - only Scala-2 macros can override Scala-2 macros")
      else if (!compatibleTypes(memberTp(self), otherTp(self)) &&
                 !compatibleTypes(memberTp(upwardsSelf), otherTp(upwardsSelf)))
        overrideError("has incompatible type" + err.whyNoMatchStr(memberTp(self), otherTp(self)))
      else if (member.erasedName != other.erasedName)
        if (other.erasedName != other.name)
          overrideError(i"needs to be declared with @alpha(${"\""}${other.erasedName}${"\""}) so that external names match")
        else
          overrideError("cannot have an @alpha annotation since external names would be different")
      else
        checkOverrideDeprecated()
    }

    /* TODO enable; right now the annotation is scala-private, so cannot be seen
         * here.
         */
    def checkOverrideDeprecated() = { /*
          if (other.hasDeprecatedOverridingAnnotation) {
            val suffix = other.deprecatedOverridingMessage map (": " + _) getOrElse ""
            val msg = s"overriding ${other.fullLocationString} is deprecated$suffix"
            unit.deprecationWarning(member.pos, msg)
          }*/
    }

    val opc = new OverridingPairs.Cursor(clazz)
    while opc.hasNext do
      checkOverride(opc.overriding, opc.overridden)
      opc.next()
    printMixinOverrideErrors()

    // Verifying a concrete class has nothing unimplemented.
    if (!clazz.isOneOf(AbstractOrTrait)) {
      val abstractErrors = new mutable.ListBuffer[String]
      def abstractErrorMessage =
        // a little formatting polish
        if (abstractErrors.size <= 2) abstractErrors mkString " "
        else abstractErrors.tail.mkString(abstractErrors.head + ":\n", "\n", "")

      def abstractClassError(mustBeMixin: Boolean, msg: String): Unit = {
        def prelude = (
          if (clazz.isAnonymousClass || clazz.is(Module)) "object creation impossible"
          else if (mustBeMixin) s"$clazz needs to be a mixin"
          else s"$clazz needs to be abstract") + ", since"

        if (abstractErrors.isEmpty) abstractErrors ++= List(prelude, msg)
        else abstractErrors += msg
      }

      def hasJavaErasedOverriding(sym: Symbol): Boolean =
        !erasurePhase.exists || // can't do the test, assume the best
          atPhase(erasurePhase.next) {
            clazz.info.nonPrivateMember(sym.name).hasAltWith { alt =>
              alt.symbol.is(JavaDefined, butNot = Deferred) &&
                !sym.owner.derivesFrom(alt.symbol.owner) &&
                alt.matches(sym)
            }
        }

      def ignoreDeferred(mbr: Symbol) =
        mbr.isType
        || mbr.isSuperAccessor // not yet synthesized
        || mbr.is(JavaDefined) && hasJavaErasedOverriding(mbr)

      def isImplemented(mbr: Symbol) =
        val mbrType = clazz.thisType.memberInfo(mbr)
        extension (sym: Symbol) def isConcrete = sym.exists && !sym.is(Deferred)
        clazz.nonPrivateMembersNamed(mbr.name)
          .filterWithPredicate(
            impl => impl.symbol.isConcrete && mbrType.matchesLoosely(impl.info))
          .exists

      /** The term symbols in this class and its baseclasses that are
       *  abstract in this class. We can't use memberNames for that since
       *  a concrete member might have the same signature as an abstract
       *  member in a base class, yet might not override it.
       */
      def missingTermSymbols: List[Symbol] =
        val buf = new mutable.ListBuffer[Symbol]
        for bc <- clazz.baseClasses
            sym <- bc.info.decls.toList
            if sym.is(DeferredTerm) && !isImplemented(sym) && !ignoreDeferred(sym)
        do buf += sym
        buf.toList

      // 2. Check that only abstract classes have deferred members
      def checkNoAbstractMembers(): Unit = {
        // Avoid spurious duplicates: first gather any missing members.
        val missing = missingTermSymbols
        // Group missing members by the name of the underlying symbol,
        // to consolidate getters and setters.
        val grouped = missing.groupBy(_.underlyingSymbol.name)

        val missingMethods = grouped.toList flatMap {
          case (name, syms) =>
            val withoutSetters = syms filterNot (_.isSetter)
            if (withoutSetters.nonEmpty) withoutSetters else syms
        }

        def stubImplementations: List[String] = {
          // Grouping missing methods by the declaring class
          val regrouped = missingMethods.groupBy(_.owner).toList
          def membersStrings(members: List[Symbol]) =
            members.sortBy(_.name.toString).map(_.showDcl + " = ???")

          if (regrouped.tail.isEmpty)
            membersStrings(regrouped.head._2)
          else (regrouped.sortBy("" + _._1.name) flatMap {
            case (owner, members) =>
              ("// Members declared in " + owner.fullName) +: membersStrings(members) :+ ""
          }).init
        }

        // If there are numerous missing methods, we presume they are aware of it and
        // give them a nicely formatted set of method signatures for implementing.
        if (missingMethods.size > 1) {
          abstractClassError(false, "it has " + missingMethods.size + " unimplemented members.")
          val preface =
            """|/** As seen from %s, the missing signatures are as follows.
                 | *  For convenience, these are usable as stub implementations.
                 | */
                 |""".stripMargin.format(clazz)
          abstractErrors += stubImplementations.map("  " + _ + "\n").mkString(preface, "", "")
          return
        }

        for (member <- missing) {
          def undefined(msg: String) =
            abstractClassError(false, s"${member.showDcl} is not defined $msg")
          val underlying = member.underlyingSymbol

          // Give a specific error message for abstract vars based on why it fails:
          // It could be unimplemented, have only one accessor, or be uninitialized.
          if (underlying.is(Mutable)) {
            val isMultiple = grouped.getOrElse(underlying.name, Nil).size > 1

            // If both getter and setter are missing, squelch the setter error.
            if (member.isSetter && isMultiple) ()
            else undefined(
              if (member.isSetter) "\n(Note that an abstract var requires a setter in addition to the getter)"
              else if (member.isGetter && !isMultiple) "\n(Note that an abstract var requires a getter in addition to the setter)"
              else err.abstractVarMessage(member))
          }
          else if (underlying.is(Method)) {
            // If there is a concrete method whose name matches the unimplemented
            // abstract method, and a cursory examination of the difference reveals
            // something obvious to us, let's make it more obvious to them.
            val abstractParams = underlying.info.firstParamTypes
            val matchingName = clazz.info.nonPrivateMember(underlying.name).alternatives
            val matchingArity = matchingName filter { m =>
              !m.symbol.is(Deferred) &&
                m.info.firstParamTypes.length == abstractParams.length
            }

            matchingArity match {
              // So far so good: only one candidate method
              case concrete :: Nil =>
                val mismatches =
                  abstractParams.zip(concrete.info.firstParamTypes)
                    .filterNot { case (x, y) => x =:= y }
                mismatches match {
                  // Only one mismatched parameter: say something useful.
                  case (pa, pc) :: Nil =>
                    val abstractSym = pa.typeSymbol
                    val concreteSym = pc.typeSymbol
                    def subclassMsg(c1: Symbol, c2: Symbol) =
                      s": ${c1.showLocated} is a subclass of ${c2.showLocated}, but method parameter types must match exactly."
                    val addendum =
                      if (abstractSym == concreteSym)
                        (pa.typeConstructor, pc.typeConstructor) match {
                          case (TypeRef(pre1, _), TypeRef(pre2, _)) =>
                            if (pre1 =:= pre2) ": their type parameters differ"
                            else ": their prefixes (i.e. enclosing instances) differ"
                          case _ =>
                            ""
                        }
                      else if (abstractSym isSubClass concreteSym)
                        subclassMsg(abstractSym, concreteSym)
                      else if (concreteSym isSubClass abstractSym)
                        subclassMsg(concreteSym, abstractSym)
                      else ""

                    undefined(s"\n(Note that ${pa.show} does not match ${pc.show}$addendum)")
                  case xs =>
                    undefined(s"\n(The class implements a member with a different type: ${concrete.showDcl})")
                }
              case Nil =>
                undefined("")
              case concretes =>
                undefined(s"\n(The class implements members with different types: ${concretes.map(_.showDcl)}%\n  %)")
            }
          }
          else undefined("")
        }
      }

      // 3. Check that concrete classes do not have deferred definitions
      // that are not implemented in a subclass.
      // Note that this is not the same as (2); In a situation like
      //
      // class C { def m: Int = 0}
      // class D extends C { def m: Int }
      //
      // (3) is violated but not (2).
      def checkNoAbstractDecls(bc: Symbol): Unit = {
        for (decl <- bc.info.decls)
          if (decl.is(Deferred)) {
            val impl = decl.matchingMember(clazz.thisType)
            if (impl == NoSymbol || decl.owner.isSubClass(impl.owner))
               && !ignoreDeferred(decl)
            then
              val impl1 = clazz.thisType.nonPrivateMember(decl.name) // DEBUG
              report.log(i"${impl1}: ${impl1.info}") // DEBUG
              report.log(i"${clazz.thisType.memberInfo(decl)}") // DEBUG
              abstractClassError(false, "there is a deferred declaration of " + infoString(decl) +
                " which is not implemented in a subclass" + err.abstractVarMessage(decl))
          }
        if (bc.asClass.superClass.is(Abstract))
          checkNoAbstractDecls(bc.asClass.superClass)
      }

      // Check that every term member of this concrete class has a symbol that matches the member's type
      // Member types are computed by intersecting the types of all members that have the same name
      // and signature. But a member selection will pick one particular implementation, according to
      // the rules of overriding and linearization. This method checks that the implementation has indeed
      // a type that subsumes the full member type.
      def checkMemberTypesOK() = {

        // First compute all member names we need to check in `membersToCheck`.
        // We do not check
        //  - types
        //  - synthetic members or bridges
        //  - members in other concrete classes, since these have been checked before
        //    (this is done for efficiency)
        //  - members in a prefix of inherited parents that all come from Java or Scala2
        //    (this is done to avoid false positives since Scala2's rules for checking are different)
        val membersToCheck = new util.HashSet[Name](4096)
        val seenClasses = new util.HashSet[Symbol](256)
        def addDecls(cls: Symbol): Unit =
          if (!seenClasses.contains(cls)) {
            seenClasses.addEntry(cls)
            for (mbr <- cls.info.decls)
              if (mbr.isTerm && !mbr.isOneOf(Synthetic | Bridge) && mbr.memberCanMatchInheritedSymbols &&
                  !membersToCheck.contains(mbr.name))
                membersToCheck.addEntry(mbr.name)
            cls.info.parents.map(_.classSymbol)
              .filter(_.isOneOf(AbstractOrTrait))
              .dropWhile(_.isOneOf(JavaDefined | Scala2x))
              .foreach(addDecls)
          }
        addDecls(clazz)

        // For each member, check that the type of its symbol, as seen from `self`
        // can override the info of this member
        for (name <- membersToCheck)
          for (mbrd <- self.member(name).alternatives) {
            val mbr = mbrd.symbol
            val mbrType = mbr.info.asSeenFrom(self, mbr.owner)
            if (!mbrType.overrides(mbrd.info, matchLoosely = true))
              report.errorOrMigrationWarning(
                em"""${mbr.showLocated} is not a legal implementation of `$name` in $clazz
                    |  its type             $mbrType
                    |  does not conform to  ${mbrd.info}""",
                (if (mbr.owner == clazz) mbr else clazz).sourcePos)
          }
      }

      /** Check that inheriting a case class does not constitute a variant refinement
       *  of a base type of the case class. It is because of this restriction that we
       *  can assume invariant refinement for case classes in `constrainPatternType`.
       */
      def checkCaseClassInheritanceInvariant() =
        for (caseCls <- clazz.info.baseClasses.tail.find(_.is(Case)))
          for (baseCls <- caseCls.info.baseClasses.tail)
            if (baseCls.typeParams.exists(_.paramVarianceSign != 0))
              for (problem <- variantInheritanceProblems(baseCls, caseCls, "non-variant", "case "))
                report.errorOrMigrationWarning(problem(), clazz.sourcePos)
      checkNoAbstractMembers()
      if (abstractErrors.isEmpty)
        checkNoAbstractDecls(clazz)

      if (abstractErrors.nonEmpty)
        report.error(abstractErrorMessage, clazz.sourcePos)

      checkMemberTypesOK()
      checkCaseClassInheritanceInvariant()
    }
    else if (clazz.is(Trait) && !(clazz derivesFrom defn.AnyValClass))
      // For non-AnyVal classes, prevent abstract methods in interfaces that override
      // final members in Object; see #4431
      for (decl <- clazz.info.decls) {
        // Have to use matchingSymbol, not a method involving overridden symbols,
        // because the scala type system understands that an abstract method here does not
        // override a concrete method in Object. The jvm, however, does not.
        val overridden = decl.matchingDecl(defn.ObjectClass, defn.ObjectType)
        if (overridden.is(Final))
          report.error(TraitRedefinedFinalMethodFromAnyRef(overridden), decl.sourcePos)
      }

    if (!clazz.is(Trait)) {
      // check that parameterized base classes and traits are typed in the same way as from the superclass
      // I.e. say we have
      //
      //    Sub extends Super extends* Base
      //
      // where `Base` has value parameters. Enforce that
      //
      //    Sub.thisType.baseType(Base)  =:=  Sub.thisType.baseType(Super).baseType(Base)
      //
      // This is necessary because parameter values are determined directly or indirectly
      // by `Super`. So we cannot pretend they have a different type when seen from `Sub`.
      def checkParameterizedTraitsOK() = {
        val mixins = clazz.mixins
        for {
          cls <- clazz.info.baseClasses.tail
          if cls.paramAccessors.nonEmpty && !mixins.contains(cls)
          problem <- variantInheritanceProblems(cls, clazz.asClass.superClass, "parameterized", "super")
        }
        report.error(problem(), clazz.sourcePos)
      }

      checkParameterizedTraitsOK()
    }

    /** Check that `site` does not inherit conflicting generic instances of `baseCls`,
     *  when doing a direct base type or going via intermediate class `middle`. I.e, we require:
     *
     *     site.baseType(baseCls)  =:=  site.baseType(middle).baseType(baseCls)
     *
     *  Return an optional by name error message if this test fails.
     */
    def variantInheritanceProblems(
        baseCls: Symbol, middle: Symbol, baseStr: String, middleStr: String): Option[() => String] = {
      val superBT = self.baseType(middle)
      val thisBT = self.baseType(baseCls)
      val combinedBT = superBT.baseType(baseCls)
      if (combinedBT =:= thisBT) None // ok
      else
        Some(() =>
          em"""illegal inheritance: $clazz inherits conflicting instances of $baseStr base $baseCls.
              |
              |  Direct basetype: $thisBT
              |  Basetype via $middleStr$middle: $combinedBT""")
    }

    /* Returns whether there is a symbol declared in class `inclazz`
       * (which must be different from `clazz`) whose name and type
       * seen as a member of `class.thisType` matches `member`'s.
       */
    def hasMatchingSym(inclazz: Symbol, member: Symbol): Boolean = {

      def isSignatureMatch(sym: Symbol) = !sym.isTerm ||
        clazz.thisType.memberInfo(sym).matchesLoosely(member.info)

      /* The rules for accessing members which have an access boundary are more
         * restrictive in java than scala.  Since java has no concept of package nesting,
         * a member with "default" (package-level) access can only be accessed by members
         * in the exact same package.  Example:
         *
         *   package a.b;
         *   public class JavaClass { void foo() { } }
         *
         * The member foo() can be accessed only from members of package a.b, and not
         * nested packages like a.b.c.  In the analogous scala class:
         *
         *   package a.b
         *   class ScalaClass { private[b] def foo() = () }
         *
         * The member IS accessible to classes in package a.b.c.  The javaAccessCheck logic
         * is restricting the set of matching signatures according to the above semantics.
         */
      def javaAccessCheck(sym: Symbol) = (
        !inclazz.is(JavaDefined) // not a java defined member
        || !sym.privateWithin.exists // no access boundary
        || sym.is(Protected) // marked protected in java, thus accessible to subclasses
        || sym.privateWithin == member.enclosingPackageClass // exact package match
        )
      def classDecls = inclazz.info.nonPrivateDecl(member.name)

      (inclazz != clazz) &&
        classDecls.hasAltWith(d => isSignatureMatch(d.symbol) && javaAccessCheck(d.symbol))
    }

    // 4. Check that every defined member with an `override` modifier overrides some other member.
    for (member <- clazz.info.decls)
      if (member.isAnyOverride && !(clazz.thisType.baseClasses exists (hasMatchingSym(_, member)))) {
        if (checks != noPrinter)
          for (bc <- clazz.info.baseClasses.tail) {
            val sym = bc.info.decl(member.name).symbol
            if (sym.exists)
              checks.println(i"$bc has $sym: ${clazz.thisType.memberInfo(sym)}")
          }

        val nonMatching = clazz.info.member(member.name).altsWith(alt => alt.owner != clazz)
        nonMatching match {
          case Nil =>
            report.error(OverridesNothing(member), member.sourcePos)
          case ms =>
            // getClass in primitive value classes is defined in the standard library as:
            //     override def getClass(): Class[Int] = ???
            // However, it's not actually an override in Dotty because our Any#getClass
            // is polymorphic (see `Definitions#Any_getClass`), so since we can't change
            // the standard library, we need to drop the override flag without reporting
            // an error.
            if (!(member.name == nme.getClass_ && clazz.isPrimitiveValueClass))
              report.error(OverridesNothingButNameExists(member, ms), member.sourcePos)
        }
        member.resetFlag(Override)
        member.resetFlag(AbsOverride)
      }
  }

  // Note: if a symbol has both @deprecated and @migration annotations and both
  // warnings are enabled, only the first one checked here will be emitted.
  // I assume that's a consequence of some code trying to avoid noise by suppressing
  // warnings after the first, but I think it'd be better if we didn't have to
  // arbitrarily choose one as more important than the other.
  private def checkUndesiredProperties(sym: Symbol, pos: SourcePosition)(using Ctx, CState): Unit =
    checkDeprecated(sym, pos)

    val xMigrationValue = ctx.settings.Xmigration.value
    if xMigrationValue != NoScalaVersion then
      checkMigration(sym, pos, xMigrationValue)


  /** If @deprecated is present, and the point of reference is not enclosed
   * in either a deprecated member or a scala bridge method, issue a warning.
   */
  private def checkDeprecated(sym: Symbol, pos: SourcePosition)(using Ctx, CState): Unit =
    for
      annot <- sym.getAnnotation(defn.DeprecatedAnnot)
      if !ctx.owner.ownersIterator.exists(_.isDeprecated)
    do
      val msg = annot.argumentConstant(0).map(": " + _.stringValue).getOrElse("")
      val since = annot.argumentConstant(1).map(" since " + _.stringValue).getOrElse("")
      report.deprecationWarning(s"${sym.showLocated} is deprecated${since}${msg}", pos)

  /** If @migration is present (indicating that the symbol has changed semantics between versions),
   *  emit a warning.
   */
  private def checkMigration(sym: Symbol, pos: SourcePosition, xMigrationValue: ScalaVersion)(using Ctx, CState): Unit =
    for annot <- sym.getAnnotation(defn.MigrationAnnot) do
      val migrationVersion = ScalaVersion.parse(annot.argumentConstant(1).get.stringValue)
      migrationVersion match
        case Success(symVersion) if xMigrationValue < symVersion =>
          val msg = annot.argumentConstant(0).get.stringValue
          report.warning(SymbolChangedSemanticsInVersion(sym, symVersion, msg), pos)
        case Failure(ex) =>
          report.warning(SymbolHasUnparsableVersionNumber(sym, ex.getMessage), pos)
        case _ =>

  /** Check that a deprecated val or def does not override a
   *  concrete, non-deprecated method.  If it does, then
   *  deprecation is meaningless.
   */
  private def checkDeprecatedOvers(tree: Tree)(using Ctx, CState): Unit = {
    val symbol = tree.symbol
    if (symbol.isDeprecated) {
      val concrOvers =
        symbol.allOverriddenSymbols.filter(sym =>
          !sym.isDeprecated && !sym.is(Deferred))
      if (!concrOvers.isEmpty)
        report.deprecationWarning(
          symbol.toString + " overrides concrete, non-deprecated symbol(s):" +
            concrOvers.map(_.name).mkString("    ", ", ", ""), tree.sourcePos)
    }
  }

  /** Check that we do not "override" anything with a private method
   *  or something that becomes a private method. According to the Scala
   *  modeling this is non-sensical since private members don't override.
   *  But Java and the JVM disagree, if the private member is a method.
   *  A test case is neg/i7926b.scala.
   *  Note: The compiler could possibly silently rename the offending private
   *  instead of flagging it as an error. But that might mean we see some
   *  surprising names at runtime. E.g. in neg/i4564a.scala, a private
   *  case class `apply` method would have to be renamed to something else.
   */
  def checkNoPrivateOverrides(tree: Tree)(using Ctx, CState): Unit =
    val sym = tree.symbol
    if sym.owner.isClass
       && sym.is(Private)
       && (sym.isOneOf(MethodOrLazyOrMutable) || !sym.is(Local)) // in these cases we'll produce a getter later
       && !sym.isConstructor
    then
      val cls = sym.owner.asClass
      for bc <- cls.baseClasses.tail do
        val other = sym.matchingDecl(bc, cls.thisType)
        if other.exists then
          report.error(i"private $sym cannot override ${other.showLocated}", sym.sourcePos)
  end checkNoPrivateOverrides

  type LevelAndIndex = immutable.Map[Symbol, (LevelInfo, Int)]

  class OptLevelInfo {
    def levelAndIndex: LevelAndIndex = Map()
    def enterReference(sym: Symbol, span: Span): Unit = ()
  }

  /** A class to help in forward reference checking */
  class LevelInfo(outerLevelAndIndex: LevelAndIndex, stats: List[Tree])(using Ctx, CState)
  extends OptLevelInfo {
    override val levelAndIndex: LevelAndIndex =
      stats.foldLeft(outerLevelAndIndex, 0) {(mi, stat) =>
        val (m, idx) = mi
        val m1 = stat match {
          case stat: MemberDef => m.updated(stat.symbol, (this, idx))
          case _ => m
        }
        (m1, idx + 1)
      }._1
    var maxIndex: Int = Int.MinValue
    var refSpan: Span = _
    var refSym: Symbol = _

    override def enterReference(sym: Symbol, span: Span): Unit =
      if (sym.exists && sym.owner.isTerm)
        levelAndIndex.get(sym) match {
          case Some((level, idx)) if (level.maxIndex < idx) =>
            level.maxIndex = idx
            level.refSpan = span
            level.refSym = sym
          case _ =>
        }
  }

  val NoLevelInfo: RefChecks.OptLevelInfo = new OptLevelInfo()
}
import RefChecks._

/** Post-attribution checking and transformation, which fulfills the following roles
 *
 *  1. This phase performs the following checks.
 *
 *  - only one overloaded alternative defines default arguments
 *  - applyDynamic methods are not overloaded
 *  - all overrides conform to rules laid down by `checkAllOverrides`.
 *  - any value classes conform to rules laid down by `checkDerivedValueClass`.
 *  - this(...) constructor calls do not forward reference other definitions in their block (not even lazy vals).
 *  - no forward reference in a local block jumps over a non-lazy val definition.
 *  - a class and its companion object do not both define a class or module with the same name.
 *
 *  2. It warns about references to symbols labeled deprecated or migration.

 *  3. It eliminates macro definitions.
 *
 *  4. It makes members not private where necessary. The following members
 *  cannot be private in the Java model:
 *   - term members of traits
 *   - the primary constructor of a value class
 *   - the parameter accessor of a value class
 *   - members accessed from an inner or companion class.
 *  All these members are marked as NotJavaPrivate.
 *  Unlike in Scala 2.x not-private members keep their name. It is
 *  up to the backend to find a unique expanded name for them. The
 *  rationale to do name changes that late is that they are very fragile.

 *  todo: But RefChecks is not done yet. It's still a somewhat dirty port from the Scala 2 version.
 *  todo: move untrivial logic to their own mini-phases
 */
class RefChecks extends MiniPhase { thisPhase =>

  import tpd._

  override def phaseName: String = RefChecks.name

  // Needs to run after ElimRepeated for override checks involving varargs methods
  override def runsAfter: Set[String] = Set(ElimRepeated.name)

  private var LevelInfo: Store.Location[OptLevelInfo] = _
  private def currentLevel(using Ctx): OptLevelInfo = ctx.store(LevelInfo)

  override def initContext(ctx: FreshCtx): Unit =
    LevelInfo = ctx.addLocation(NoLevelInfo)

  override def prepareForStats(trees: List[Tree])(using Ctx, CState): Ctx =
    if (ctx.owner.isTerm)
      ctx.fresh.updateStore(LevelInfo, new LevelInfo(currentLevel.levelAndIndex, trees))
    else ctx

  override def transformValDef(tree: ValDef)(using Ctx, CState): ValDef = {
    checkNoPrivateOverrides(tree)
    checkDeprecatedOvers(tree)
    val sym = tree.symbol
    if (sym.exists && sym.owner.isTerm) {
      tree.rhs match {
        case Ident(nme.WILDCARD) => report.error(UnboundPlaceholderParameter(), sym.sourcePos)
        case _ =>
      }
      if (!sym.is(Lazy))
        currentLevel.levelAndIndex.get(sym) match {
          case Some((level, symIdx)) if symIdx <= level.maxIndex =>
            report.error(ForwardReferenceExtendsOverDefinition(sym, level.refSym),
              ctx.source.atSpan(level.refSpan))
          case _ =>
        }
    }
    tree
  }

  override def transformDefDef(tree: DefDef)(using Ctx, CState): DefDef = {
    checkNoPrivateOverrides(tree)
    checkDeprecatedOvers(tree)
    tree
  }

  override def transformTemplate(tree: Template)(using Ctx, CState): Tree = try {
    val cls = ctx.owner.asClass
    checkOverloadedRestrictions(cls)
    checkParents(cls)
    if (cls.is(Trait)) tree.parents.foreach(checkParentPrefix(cls, _))
    checkCompanionNameClashes(cls)
    checkAllOverrides(cls)
    tree
  }
  catch {
    case ex: TypeError =>
      report.error(ex, tree.sourcePos)
      tree
  }

  override def transformIdent(tree: Ident)(using Ctx, CState): Ident = {
    checkUndesiredProperties(tree.symbol, tree.sourcePos)
    currentLevel.enterReference(tree.symbol, tree.span)
    tree
  }

  override def transformSelect(tree: Select)(using Ctx, CState): Select = {
    checkUndesiredProperties(tree.symbol, tree.sourcePos)
    tree
  }

  override def transformApply(tree: Apply)(using Ctx, CState): Apply = {
    if (isSelfConstrCall(tree)) {
      assert(currentLevel.isInstanceOf[LevelInfo], s"${ctx.owner}/" + i"$tree")
      val level = currentLevel.asInstanceOf[LevelInfo]
      if (level.maxIndex > 0) {
        // An implementation restriction to avoid VerifyErrors and lazyvals mishaps; see SI-4717
        report.debuglog("refsym = " + level.refSym)
        report.error("forward reference not allowed from self constructor invocation",
          ctx.source.atSpan(level.refSpan))
      }
    }
    tree
  }

  override def transformNew(tree: New)(using Ctx, CState): New = {
    val tpe = tree.tpe
    val sym = tpe.typeSymbol
    checkUndesiredProperties(sym, tree.sourcePos)
    currentLevel.enterReference(sym, tree.span)
    tpe.dealias.foreachPart {
      case TermRef(_, s: Symbol) => currentLevel.enterReference(s, tree.span)
      case _ =>
    }
    tree
  }
}

/* todo: rewrite and re-enable

// Comparison checking -------------------------------------------------------

    object normalizeAll extends TypeMap {
      def apply(tp: Type) = mapOver(tp).normalize
    }

    def checkImplicitViewOptionApply(pos: Position, fn: Tree, args: List[Tree]): Unit = if (settings.lint) (fn, args) match {
      case (tap@TypeApply(fun, targs), List(view: ApplyImplicitView)) if fun.symbol == currentRun.runDefinitions.Option_apply =>
        unit.warning(pos, s"Suspicious application of an implicit view (${view.fun}) in the argument to Option.apply.") // SI-6567
      case _ =>
    }

    private def isObjectOrAnyComparisonMethod(sym: Symbol) = sym match {
      case Object_eq | Object_ne | Object_== | Object_!= | Any_== | Any_!= => true
      case _                                                               => false
    }
    /** Check the sensibility of using the given `equals` to compare `qual` and `other`. */
    private def checkSensibleEquals(pos: Position, qual: Tree, name: Name, sym: Symbol, other: Tree) = {
      def isReferenceOp = sym == Object_eq || sym == Object_ne
      def isNew(tree: Tree) = tree match {
        case Function(_, _) | Apply(Select(New(_), nme.CONSTRUCTOR), _) => true
        case _ => false
      }
      def underlyingClass(tp: Type): Symbol = {
        val sym = tp.widen.typeSymbol
        if (sym.isAbstractOrParamType) underlyingClass(sym.info.bounds.hi)
        else sym
      }
      val actual   = underlyingClass(other.tpe)
      val receiver = underlyingClass(qual.tpe)
      def onTrees[T](f: List[Tree] => T) = f(List(qual, other))
      def onSyms[T](f: List[Symbol] => T) = f(List(receiver, actual))

      // @MAT normalize for consistency in error message, otherwise only part is normalized due to use of `typeSymbol`
      def typesString = normalizeAll(qual.tpe.widen)+" and " + normalizeAll(other.tpe.widen)

      /* Symbols which limit the warnings we can issue since they may be value types */
      val isMaybeValue = Set[Symbol](AnyClass, AnyRefClass, AnyValClass, ObjectClass, ComparableClass, JavaSerializableClass)

      // Whether def equals(other: Any) has known behavior: it is the default
      // inherited from java.lang.Object, or it is a synthetically generated
      // case equals.  TODO - more cases are warnable if the target is a synthetic
      // equals.
      def isUsingWarnableEquals = {
        val m = receiver.info.member(nme.equals_)
        ((m == Object_equals) || (m == Any_equals) || isMethodCaseEquals(m))
      }
      def isMethodCaseEquals(m: Symbol) = m.isSynthetic && m.owner.isCase
      def isCaseEquals = isMethodCaseEquals(receiver.info.member(nme.equals_))
      // Whether this == or != is one of those defined in Any/AnyRef or an overload from elsewhere.
      def isUsingDefaultScalaOp = sym == Object_== || sym == Object_!= || sym == Any_== || sym == Any_!=
      def haveSubclassRelationship = (actual isSubClass receiver) || (receiver isSubClass actual)

      // Whether the operands+operator represent a warnable combo (assuming anyrefs)
      // Looking for comparisons performed with ==/!= in combination with either an
      // equals method inherited from Object or a case class synthetic equals (for
      // which we know the logic.)
      def isWarnable           = isReferenceOp || (isUsingDefaultScalaOp && isUsingWarnableEquals)
      def isEitherNullable     = (NullTpe <:< receiver.info) || (NullTpe <:< actual.info)
      def isEitherValueClass   = actual.isDerivedValueClass || receiver.isDerivedValueClass
      def isBoolean(s: Symbol) = unboxedValueClass(s) == BooleanClass
      def isUnit(s: Symbol)    = unboxedValueClass(s) == UnitClass
      def isNumeric(s: Symbol) = isNumericValueClass(unboxedValueClass(s)) || isAnyNumber(s)
      def isScalaNumber(s: Symbol) = s isSubClass ScalaNumberClass
      def isJavaNumber(s: Symbol)  = s isSubClass JavaNumberClass
      // includes java.lang.Number if appropriate [SI-5779]
      def isAnyNumber(s: Symbol)     = isScalaNumber(s) || isJavaNumber(s)
      def isMaybeAnyValue(s: Symbol) = isPrimitiveValueClass(unboxedValueClass(s)) || isMaybeValue(s)
      // used to short-circuit unrelatedTypes check if both sides are special
      def isSpecial(s: Symbol) = isMaybeAnyValue(s) || isAnyNumber(s)
      val nullCount            = onSyms(_ filter (_ == NullClass) size)
      def isNonsenseValueClassCompare = (
           !haveSubclassRelationship
        && isUsingDefaultScalaOp
        && isEitherValueClass
        && !isCaseEquals
      )

      // Have we already determined that the comparison is non-sensible? I mean, non-sensical?
      var isNonSensible = false

      def nonSensibleWarning(what: String, alwaysEqual: Boolean) = {
        val msg = alwaysEqual == (name == nme.EQ || name == nme.eq)
        unit.warning(pos, s"comparing $what using `${name.decode}` will always yield $msg")
        isNonSensible = true
      }
      def nonSensible(pre: String, alwaysEqual: Boolean) =
        nonSensibleWarning(s"${pre}values of types $typesString", alwaysEqual)
      def nonSensiblyEq() = nonSensible("", alwaysEqual = true)
      def nonSensiblyNeq() = nonSensible("", alwaysEqual = false)
      def nonSensiblyNew() = nonSensibleWarning("a fresh object", alwaysEqual = false)

      def unrelatedMsg = name match {
        case nme.EQ | nme.eq => "never compare equal"
        case _               => "always compare unequal"
      }
      def unrelatedTypes() = if (!isNonSensible) {
        val weaselWord = if (isEitherValueClass) "" else " most likely"
        unit.warning(pos, s"$typesString are unrelated: they will$weaselWord $unrelatedMsg")
      }

      if (nullCount == 2) // null == null
        nonSensiblyEq()
      else if (nullCount == 1) {
        if (onSyms(_ exists isPrimitiveValueClass)) // null == 5
          nonSensiblyNeq()
        else if (onTrees( _ exists isNew)) // null == new AnyRef
          nonSensiblyNew()
      }
      else if (isBoolean(receiver)) {
        if (!isBoolean(actual) && !isMaybeValue(actual))    // true == 5
          nonSensiblyNeq()
      }
      else if (isUnit(receiver)) {
        if (isUnit(actual)) // () == ()
          nonSensiblyEq()
        else if (!isUnit(actual) && !isMaybeValue(actual))  // () == "abc"
          nonSensiblyNeq()
      }
      else if (isNumeric(receiver)) {
        if (!isNumeric(actual))
          if (isUnit(actual) || isBoolean(actual) || !isMaybeValue(actual))   // 5 == "abc"
            nonSensiblyNeq()
      }
      else if (isWarnable && !isCaseEquals) {
        if (isNew(qual)) // new X == y
          nonSensiblyNew()
        else if (isNew(other) && (receiver.isEffectivelyFinal || isReferenceOp))   // object X ; X == new Y
          nonSensiblyNew()
        else if (receiver.isEffectivelyFinal && !(receiver isSubClass actual) && !actual.isRefinementClass) {  // object X, Y; X == Y
          if (isEitherNullable)
            nonSensible("non-null ", false)
          else
            nonSensiblyNeq()
        }
      }

      // warn if one but not the other is a derived value class
      // this is especially important to enable transitioning from
      // regular to value classes without silent failures.
      if (isNonsenseValueClassCompare)
        unrelatedTypes()
      // possibleNumericCount is insufficient or this will warn on e.g. Boolean == j.l.Boolean
      else if (isWarnable && nullCount == 0 && !(isSpecial(receiver) && isSpecial(actual))) {
        // better to have lubbed and lost
        def warnIfLubless(): Unit = {
          val common = global.lub(List(actual.tpe, receiver.tpe))
          if (ObjectTpe <:< common)
            unrelatedTypes()
        }
        // warn if actual has a case parent that is not same as receiver's;
        // if actual is not a case, then warn if no common supertype, as below
        if (isCaseEquals) {
          def thisCase = receiver.info.member(nme.equals_).owner
          actual.info.baseClasses.find(_.isCase) match {
            case Some(p) if p != thisCase => nonSensible("case class ", false)
            case None =>
              // stronger message on (Some(1) == None)
              //if (receiver.isCase && receiver.isEffectivelyFinal && !(receiver isSubClass actual)) nonSensiblyNeq()
              //else
              // if a class, it must be super to thisCase (and receiver) since not <: thisCase
              if (!actual.isTrait && !(receiver isSubClass actual)) nonSensiblyNeq()
              else if (!haveSubclassRelationship) warnIfLubless()
            case _ =>
          }
        }
        // warn only if they have no common supertype below Object
        else if (!haveSubclassRelationship) {
          warnIfLubless()
        }
      }
    }
    /** Sensibility check examines flavors of equals. */
    def checkSensible(pos: Position, fn: Tree, args: List[Tree]) = fn match {
      case Select(qual, name @ (nme.EQ | nme.NE | nme.eq | nme.ne)) if args.length == 1 && isObjectOrAnyComparisonMethod(fn.symbol) =>
        checkSensibleEquals(pos, qual, name, fn.symbol, args.head)
      case _ =>
    }
*/

/* --------------- Overflow -------------------------------------------------
 *

  def accessFlagsToString(sym: Symbol) = flagsToString(
    sym getFlag (PRIVATE | PROTECTED),
    if (sym.hasAccessBoundary) "" + sym.privateWithin.name else ""
  )

  def overridesTypeInPrefix(tp1: Type, tp2: Type, prefix: Type): Boolean = (tp1.dealiasWiden, tp2.dealiasWiden) match {
    case (MethodType(List(), rtp1), NullaryMethodType(rtp2)) =>
      rtp1 <:< rtp2
    case (NullaryMethodType(rtp1), MethodType(List(), rtp2)) =>
      rtp1 <:< rtp2
    case (TypeRef(_, sym, _),  _) if sym.isModuleClass =>
      overridesTypeInPrefix(NullaryMethodType(tp1), tp2, prefix)
    case _ =>
      def classBoundAsSeen(tp: Type) = tp.typeSymbol.classBound.asSeenFrom(prefix, tp.typeSymbol.owner)

      (tp1 <:< tp2) || (  // object override check
        tp1.typeSymbol.isModuleClass && tp2.typeSymbol.isModuleClass && {
          val cb1 = classBoundAsSeen(tp1)
          val cb2 = classBoundAsSeen(tp2)
          (cb1 <:< cb2) && {
            log("Allowing %s to override %s because %s <:< %s".format(tp1, tp2, cb1, cb2))
            true
          }
        }
      )
  }
    private def checkTypeRef(tp: Type, tree: Tree, skipBounds: Boolean)(using Ctx, CState) = tp match {
      case TypeRef(pre, sym, args) =>
        tree match {
          case tt: TypeTree if tt.original == null => // SI-7783 don't warn about inferred types
                                                      // FIXME: reconcile this check with one in resetAttrs
          case _ => checkUndesiredProperties(sym, tree.pos)
        }
        if (sym.isJavaDefined)
          sym.typeParams foreach (_.cookJavaRawInfo())
        if (!tp.isHigherKinded && !skipBounds)
          checkBounds(tree, pre, sym.owner, sym.typeParams, args)
      case _ =>
    }

    private def checkTypeRefBounds(tp: Type, tree: Tree) = {
      var skipBounds = false
      tp match {
        case AnnotatedType(ann :: Nil, underlying) if ann.symbol == UncheckedBoundsClass =>
          skipBounds = true
          underlying
        case TypeRef(pre, sym, args) =>
          if (!tp.isHigherKinded && !skipBounds)
            checkBounds(tree, pre, sym.owner, sym.typeParams, args)
          tp
        case _ =>
          tp
      }
    }

    private def checkAnnotations(tpes: List[Type], tree: Tree) = tpes foreach { tp =>
      checkTypeRef(tp, tree, skipBounds = false)
      checkTypeRefBounds(tp, tree)
    }
    private def doTypeTraversal(tree: Tree)(f: Type => Unit) = if (!inPattern) tree.tpe foreach f

    private def applyRefchecksToAnnotations(tree: Tree)(using Ctx, CState): Unit = {
      def applyChecks(annots: List[Annotation]) = {
        checkAnnotations(annots map (_.atp), tree)
        transformTrees(annots flatMap (_.args))
      }

      tree match {
        case m: MemberDef =>
          val sym = m.symbol
          applyChecks(sym.annotations)
          // validate implicitNotFoundMessage
          analyzer.ImplicitNotFoundMsg.check(sym) foreach { warn =>
            unit.warning(tree.pos, f"Invalid implicitNotFound message for ${sym}%s${sym.locationString}%s:%n$warn")
          }

        case tpt@TypeTree() =>
          if (tpt.original != null) {
            tpt.original foreach {
              case dc@TypeTreeWithDeferredRefCheck() =>
                applyRefchecksToAnnotations(dc.check()) // #2416
              case _ =>
            }
          }

          doTypeTraversal(tree) {
            case tp @ AnnotatedType(annots, _)  =>
              applyChecks(annots)
            case tp =>
          }
        case _ =>
      }
    }

    private def transformCaseApply(tree: Tree, ifNot: => Unit) = {
      val sym = tree.symbol

      def isClassTypeAccessible(tree: Tree): Boolean = tree match {
        case TypeApply(fun, targs) =>
          isClassTypeAccessible(fun)
        case Select(module, apply) =>
          ( // SI-4859 `CaseClass1().InnerCaseClass2()` must not be rewritten to `new InnerCaseClass2()`;
            //          {expr; Outer}.Inner() must not be rewritten to `new Outer.Inner()`.
            treeInfo.isQualifierSafeToElide(module) &&
            // SI-5626 Classes in refinement types cannot be constructed with `new`. In this case,
            // the companion class is actually not a ClassSymbol, but a reference to an abstract type.
            module.symbol.companionClass.isClass
          )
      }

      val doTransform =
        sym.isRealMethod &&
        sym.isCase &&
        sym.name == nme.apply &&
        isClassTypeAccessible(tree)

      if (doTransform) {
        tree foreach {
          case i@Ident(_) =>
            enterReference(i.pos, i.symbol) // SI-5390 need to `enterReference` for `a` in `a.B()`
          case _ =>
        }
        toConstructor(tree.pos, tree.tpe)
      }
      else {
        ifNot
        tree
      }
    }

    private def transformApply(tree: Apply): Tree = tree match {
      case Apply(
        Select(qual, nme.filter | nme.withFilter),
        List(Function(
          List(ValDef(_, pname, tpt, _)),
          Match(_, CaseDef(pat1, _, _) :: _))))
        if ((pname startsWith nme.CHECK_IF_REFUTABLE_STRING) &&
            isIrrefutable(pat1, tpt.tpe) && (qual.tpe <:< tree.tpe)) =>

          transform(qual)

      case Apply(fn, args) =>
        // sensicality should be subsumed by the unreachability/exhaustivity/irrefutability
        // analyses in the pattern matcher
        if (!inPattern) {
          checkImplicitViewOptionApply(tree.pos, fn, args)
          checkSensible(tree.pos, fn, args)
        }
        currentApplication = tree
        tree
    }
    private def transformSelect(tree: Select): Tree = {
      val Select(qual, _) = tree
      val sym = tree.symbol

      checkUndesiredProperties(sym, tree.pos)
      checkDelayedInitSelect(qual, sym, tree.pos)

      if (!sym.exists)
        devWarning("Select node has NoSymbol! " + tree + " / " + tree.tpe)
      else if (sym.isLocalToThis)
        varianceValidator.checkForEscape(sym, currentClass)

      def checkSuper(mix: Name) =
        // term should have been eliminated by super accessors
        assert(!(qual.symbol.isTrait && sym.isTerm && mix == tpnme.EMPTY), (qual.symbol, sym, mix))

      transformCaseApply(tree,
        qual match {
          case Super(_, mix)  => checkSuper(mix)
          case _              =>
        }
      )
    }
    private def transformIf(tree: If): Tree = {
      val If(cond, thenpart, elsepart) = tree
      def unitIfEmpty(t: Tree): Tree =
        if (t == EmptyTree) Literal(Constant(())).setPos(tree.pos).setType(UnitTpe) else t

      cond.tpe match {
        case ConstantType(value) =>
          val res = if (value.booleanValue) thenpart else elsepart
          unitIfEmpty(res)
        case _ => tree
      }
    }

    // Warning about nullary methods returning Unit. TODO: move to lint
    private def checkNullaryMethodReturnType(sym: Symbol) = sym.tpe match {
      case NullaryMethodType(restpe) if restpe.typeSymbol == UnitClass =>
        // this may be the implementation of e.g. a generic method being parameterized
        // on Unit, in which case we had better let it slide.
        val isOk = (
             sym.isGetter
          || (sym.name containsName nme.DEFAULT_GETTER_STRING)
          || sym.allOverriddenSymbols.exists(over => !(over.tpe.resultType =:= sym.tpe.resultType))
        )
        if (!isOk)
          unit.warning(sym.pos, s"side-effecting nullary methods are discouraged: suggest defining as `def ${sym.name.decode}()` instead")
      case _ => ()
    }

    /* Convert a reference to a case factory of type `tpe` to a new of the class it produces. */
    def toConstructor(pos: Position, tpe: Type)(using Ctx, CState): Tree = {
      val rtpe = tpe.finalResultType
      assert(rtpe.typeSymbol.is(Case), tpe)
      New(rtpe).withPos(pos).select(rtpe.typeSymbol.primaryConstructor)
    }
    private def isIrrefutable(pat: Tree, seltpe: Type): Boolean = pat match {
      case Apply(_, args) =>
        val clazz = pat.tpe.typeSymbol
        clazz == seltpe.typeSymbol &&
        clazz.isCaseClass &&
        (args corresponds clazz.primaryConstructor.tpe.asSeenFrom(seltpe, clazz).paramTypes)(isIrrefutable)
      case Typed(pat, tpt) =>
        seltpe <:< tpt.tpe
      case Ident(tpnme.WILDCARD) =>
        true
      case Bind(_, pat) =>
        isIrrefutable(pat, seltpe)
      case _ =>
        false
    }
    private def checkDelayedInitSelect(qual: Tree, sym: Symbol, pos: Position) = {
      def isLikelyUninitialized = (
           (sym.owner isSubClass DelayedInitClass)
        && !qual.tpe.isInstanceOf[ThisType]
        && sym.accessedOrSelf.isVal
      )
      if (settings.lint.value && isLikelyUninitialized)
        unit.warning(pos, s"Selecting ${sym} from ${sym.owner}, which extends scala.DelayedInit, is likely to yield an uninitialized value")
    }
    private def lessAccessible(otherSym: Symbol, memberSym: Symbol): Boolean = (
         (otherSym != NoSymbol)
      && !otherSym.isProtected
      && !otherSym.isTypeParameterOrSkolem
      && !otherSym.isExistentiallyBound
      && (otherSym isLessAccessibleThan memberSym)
      && (otherSym isLessAccessibleThan memberSym.enclClass)
    )
    private def lessAccessibleSymsInType(other: Type, memberSym: Symbol): List[Symbol] = {
      val extras = other match {
        case TypeRef(pre, _, args) =>
          // checking the prefix here gives us spurious errors on e.g. a private[process]
          // object which contains a type alias, which normalizes to a visible type.
          args filterNot (_ eq NoPrefix) flatMap (tp => lessAccessibleSymsInType(tp, memberSym))
        case _ =>
          Nil
      }
      if (lessAccessible(other.typeSymbol, memberSym)) other.typeSymbol :: extras
      else extras
    }
    private def warnLessAccessible(otherSym: Symbol, memberSym: Symbol) {
      val comparison = accessFlagsToString(memberSym) match {
        case ""   => ""
        case acc  => " is " + acc + " but"
      }
      val cannot =
        if (memberSym.isDeferred) "may be unable to provide a concrete implementation of"
        else "may be unable to override"

      unit.warning(memberSym.pos,
        "%s%s references %s %s.".format(
          memberSym.fullLocationString, comparison,
          accessFlagsToString(otherSym), otherSym
        ) + "\nClasses which cannot access %s %s %s.".format(
          otherSym.decodedName, cannot, memberSym.decodedName)
      )
    }

    /** Warn about situations where a method signature will include a type which
     *  has more restrictive access than the method itself.
     */
    private def checkAccessibilityOfReferencedTypes(tree: Tree) {
      val member = tree.symbol

      def checkAccessibilityOfType(tpe: Type) {
        val inaccessible = lessAccessibleSymsInType(tpe, member)
        // if the unnormalized type is accessible, that's good enough
        if (inaccessible.isEmpty) ()
        // or if the normalized type is, that's good too
        else if ((tpe ne tpe.normalize) && lessAccessibleSymsInType(tpe.dealiasWiden, member).isEmpty) ()
        // otherwise warn about the inaccessible syms in the unnormalized type
        else inaccessible foreach (sym => warnLessAccessible(sym, member))
      }

      // types of the value parameters
      mapParamss(member)(p => checkAccessibilityOfType(p.tpe))
      // upper bounds of type parameters
      member.typeParams.map(_.info.bounds.hi.widen) foreach checkAccessibilityOfType
    }

    private def checkByNameRightAssociativeDef(tree: DefDef) {
      tree match {
        case DefDef(_, name, _, params :: _, _, _) =>
          if (settings.lint && !treeInfo.isLeftAssoc(name.decodedName) && params.exists(p => isByName(p.symbol)))
            unit.warning(tree.pos,
              "by-name parameters will be evaluated eagerly when called as a right-associative infix operator. For more details, see SI-1980.")
        case _ =>
      }
    }
    override def transform(tree: Tree)(using Ctx, CState): Tree = {
      //val savedLocalTyper = localTyper
      try {
        val sym = tree.symbol
        checkOverloadedRestrictions(ctx.owner)
            checkAllOverrides(ctx.owner)
            checkAnyValSubclass(ctx.owner)
            if (ctx.owner.isDerivedValueClass)
              ctx.owner.primaryConstructor.makeNotPrivateAfter(NoSymbol, thisPhase) // SI-6601, must be done *after* pickler!
            tree


        // Apply RefChecks to annotations. Makes sure the annotations conform to
        // type bounds (bug #935), issues deprecation warnings for symbols used
        // inside annotations.
        // applyRefchecksToAnnotations(tree) ???
        var result: Tree = tree match {
          case tree: ValOrDefDef =>
            // move to lint:
            // if (settings.warnNullaryUnit)
            //  checkNullaryMethodReturnType(sym)
            // if (settings.warnInaccessible) {
            //  if (!sym.isConstructor && !sym.isEffectivelyFinal && !sym.isSynthetic)
            //    checkAccessibilityOfReferencedTypes(tree)
            // }
            // tree match {
            //  case dd: DefDef => checkByNameRightAssociativeDef(dd)
            //  case _          =>
            // }
            tree

          case Template(constr, parents, self, body) =>
            // localTyper = localTyper.atOwner(tree, currentOwner)
            checkOverloadedRestrictions(ctx.owner)
            checkAllOverrides(ctx.owner)
            checkAnyValSubclass(ctx.owner)
            if (ctx.owner.isDerivedValueClass)
              ctx.owner.primaryConstructor.makeNotPrivateAfter(NoSymbol, thisPhase) // SI-6601, must be done *after* pickler!
            tree

          case tpt: TypeTree =>
            transform(tpt.original)
            tree

          case TypeApply(fn, args) =>
            checkBounds(tree, NoPrefix, NoSymbol, fn.tpe.typeParams, args map (_.tpe))
            transformCaseApply(tree, ())

          case x @ Apply(_, _)  =>
            transformApply(x)

          case x @ If(_, _, _)  =>
            transformIf(x)

          case New(tpt) =>
            enterReference(tree.pos, tpt.tpe.typeSymbol)
            tree

          case treeInfo.WildcardStarArg(_) if !isRepeatedParamArg(tree) =>
            unit.error(tree.pos, "no `: _*` annotation allowed here\n" +
              "(such annotations are only allowed in arguments to *-parameters)")
            tree

          case Ident(name) =>
            checkUndesiredProperties(sym, tree.pos)
            transformCaseApply(tree,
              if (name != nme.WILDCARD && name != tpnme.WILDCARD_STAR) {
                assert(sym != NoSymbol, "transformCaseApply: name = " + name.debugString + " tree = " + tree + " / " + tree.getClass) //debug
                enterReference(tree.pos, sym)
              }
            )

          case x @ Select(_, _) =>
            transformSelect(x)

          case UnApply(fun, args) =>
            transform(fun) // just make sure we enterReference for unapply symbols, note that super.transform(tree) would not transform(fun)
                           // transformTrees(args) // TODO: is this necessary? could there be forward references in the args??
                           // probably not, until we allow parameterised extractors
            tree


          case _ => tree
        }

        // skip refchecks in patterns....
        result = result match {
          case CaseDef(pat, guard, body) =>
            val pat1 = savingInPattern {
              inPattern = true
              transform(pat)
            }
            treeCopy.CaseDef(tree, pat1, transform(guard), transform(body))
          case LabelDef(_, _, _) if treeInfo.hasSynthCaseSymbol(result) =>
            savingInPattern {
              inPattern = true
              deriveLabelDef(result)(transform)
            }
          case Apply(fun, args) if fun.symbol.isLabel && treeInfo.isSynthCaseSymbol(fun.symbol) =>
            savingInPattern {
              // SI-7756 If we were in a translated pattern, we can now switch out of pattern mode, as the label apply signals
              //         that we are in the user-supplied code in the case body.
              //
              //         Relies on the translation of:
              //            (null: Any) match { case x: List[?] => x; x.reverse; case _ => }'
              //         to:
              //            <synthetic> val x2: List[?] = (x1.asInstanceOf[List[?]]: List[?]);
              //                  matchEnd4({ x2; x2.reverse}) // case body is an argument to a label apply.
              inPattern = false
              super.transform(result)
            }
          case ValDef(_, _, _, _) if treeInfo.hasSynthCaseSymbol(result) =>
            deriveValDef(result)(transform) // SI-7716 Don't refcheck the tpt of the synthetic val that holds the selector.
          case _ =>
            super.transform(result)
        }
        result match {
          case ClassDef(_, _, _, _)
             | TypeDef(_, _, _, _) =>
            if (result.symbol.isLocalToBlock || result.symbol.isTopLevel)
              varianceValidator.traverse(result)
          case tt @ TypeTree() if tt.original != null =>
            varianceValidator.traverse(tt.original) // See SI-7872
          case _ =>
        }

        checkUnexpandedMacro(result)

        result
      } catch {
        case ex: TypeError =>
          if (settings.debug) ex.printStackTrace()
          unit.error(tree.pos, ex.toMessage)
          tree
      } finally {
        localTyper = savedLocalTyper
        currentApplication = savedCurrentApplication
      }
    }
*/