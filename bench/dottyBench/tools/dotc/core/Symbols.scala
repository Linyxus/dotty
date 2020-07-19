package dottyBench.tools
package dotc
package core

import Periods._
import Names._
import Scopes._
import Flags._
import Decorators._
import Symbols._
import Contexts._
import Phases._
import SymDenotations._
import Denotations._
import printing.Texts._
import printing.Printer
import Types._
import util.Spans._
import DenotTransformers._
import StdNames._
import NameOps._
import transform.SymUtils._
import NameKinds.LazyImplicitName
import ast.tpd
import tpd.{Tree, TreeProvider, TreeOps}
import ast.TreeTypeMap
import Constants.Constant
import Variances.{Variance, varianceFromInt}
import reporting.Message
import collection.mutable
import io.AbstractFile
import language.implicitConversions
import util.{SourceFile, NoSource, Property, SourcePosition}
import scala.collection.JavaConverters._
import scala.annotation.internal.sharable
import config.Printers.typr

object Symbols {

  implicit def eqSymbol: Eql[Symbol, Symbol] = Eql.derived

  /** Tree attachment containing the identifiers in a tree as a sorted array */
  val Ids: Property.Key[Array[String]] = new Property.Key

  /** A Symbol represents a Scala definition/declaration or a package.
   *  @param coord  The coordinates of the symbol (a position or an index)
   *  @param id     A unique identifier of the symbol (unique per ContextBase)
   */
  class Symbol private[Symbols] (private var myCoord: Coord, val id: Int)
    extends Designator with ParamInfo with printing.Showable {

    type ThisName <: Name

    //assert(id != 723)

    def coord: Coord = myCoord
    /** Set the coordinate of this class, this is only useful when the coordinate is
     *  not known at symbol creation. This is the case for root symbols
     *  unpickled from TASTY.
     *
     *  @pre coord == NoCoord
     */
    private[core] def coord_=(c: Coord): Unit = {
      assert(myCoord == NoCoord)
      myCoord = c
    }

    private var myDefTree: Tree = null

    /** The tree defining the symbol at pickler time, EmptyTree if none was retained */
    def defTree: Tree =
      if (myDefTree == null) tpd.EmptyTree else myDefTree

    /** Set defining tree if this symbol retains its definition tree */
    def defTree_=(tree: Tree)(using Ctx, CState): Unit =
      if (retainsDefTree) myDefTree = tree

    /** Does this symbol retain its definition tree?
     *  A good policy for this needs to balance costs and benefits, where
     *  costs are mainly memoty leaks, in particular across runs.
     */
    def retainsDefTree(using Ctx, CState): Boolean =
      ctx.settings.YretainTrees.value ||
      denot.owner.isTerm ||                // no risk of leaking memory after a run for these
      denot.isOneOf(InlineOrProxy) ||      // need to keep inline info
      ctx.settings.YcheckInit.value        // initialization check

    /** The last denotation of this symbol */
    private var lastDenot: SymDenotation = _
    private var checkedPeriod: Period = Nowhere

    private[core] def invalidateDenotCache(): Unit = { checkedPeriod = Nowhere }

    /** Set the denotation of this symbol */
    private[core] def denot_=(d: SymDenotation): Unit = {
      util.Stats.record("Symbol.denot_=")
      lastDenot = d
      checkedPeriod = Nowhere
    }

    /** The current denotation of this symbol */
    final def denot(using Ctx, CState): SymDenotation = {
      util.Stats.record("Symbol.denot")
      val lastd = lastDenot
      if (checkedPeriod == currentPeriod) lastd
      else computeDenot(lastd)
    }

    private def computeDenot(lastd: SymDenotation)(using Ctx, CState): SymDenotation = {
      util.Stats.record("Symbol.computeDenot")
      val now = currentPeriod
      checkedPeriod = now
      if (lastd.validFor contains now) lastd else recomputeDenot(lastd)
    }

    /** Overridden in NoSymbol */
    protected def recomputeDenot(lastd: SymDenotation)(using Ctx, CState): SymDenotation = {
      util.Stats.record("Symbol.recomputeDenot")
      val newd = lastd.current.asInstanceOf[SymDenotation]
      lastDenot = newd
      newd
    }

    /** The original denotation of this symbol, without forcing anything */
    final def originDenotation: SymDenotation =
      lastDenot.initial

    /** The last known denotation of this symbol, without going through `current` */
    final def lastKnownDenotation: SymDenotation =
      lastDenot

    private[core] def defRunId: RunId =
      if (lastDenot == null) NoRunId else lastDenot.validFor.runId

    /** Does this symbol come from a currently compiled source file? */
    final def isDefinedInCurrentRun(using Ctx, CState): Boolean =
      span.exists && defRunId == currentRunId && {
        val file = associatedFile
        file != null && ctx.run.files.contains(file)
      }

    /** Is symbol valid in current run? */
    final def isValidInCurrentRun(using Ctx, CState): Boolean =
      (lastDenot.validFor.runId == currentRunId || stillValid(lastDenot)) &&
      (lastDenot.symbol eq this)
        // the last condition is needed because under ctx.staleOK overwritten
        // members keep denotations pointing to the new symbol, so the validity
        // periods check out OK. But once a package member is overridden it is not longer
        // valid. If the option would be removed, the check would be no longer needed.

    final def isTerm(using Ctx, CState): Boolean =
      (if (defRunId == currentRunId) lastDenot else denot).isTerm
    final def isType(using Ctx, CState): Boolean =
      (if (defRunId == currentRunId) lastDenot else denot).isType
    final def asTerm(using Ctx, CState): TermSymbol = {
      assert(isTerm, s"asTerm called on not-a-Term $this" );
      asInstanceOf[TermSymbol]
    }
    final def asType(using Ctx, CState): TypeSymbol = {
      assert(isType, s"isType called on not-a-Type $this");
      asInstanceOf[TypeSymbol]
    }

    final def isClass: Boolean = isInstanceOf[ClassSymbol]
    final def asClass: ClassSymbol = asInstanceOf[ClassSymbol]

    /** Test whether symbol is private. This
     *  conservatively returns `false` if symbol does not yet have a denotation, or denotation
     *  is a class that is not yet read.
     */
    final def isPrivate(using Ctx, CState): Boolean = {
      val d = lastDenot
      d != null && d.flagsUNSAFE.is(Private)
    }

    /** Is the symbol a pattern bound symbol?
     */
    final def isPatternBound(using Ctx, CState): Boolean =
      !isClass && this.is(Case, butNot = Enum | Module)

    /** The symbol's signature if it is completed or a method, NotAMethod otherwise. */
    final def signature(using Ctx, CState): Signature =
      if (lastDenot != null && (lastDenot.isCompleted || lastDenot.is(Method)))
        denot.signature
      else
        Signature.NotAMethod

    /** Special cased here, because it may be used on naked symbols in substituters */
    final def isStatic(using Ctx, CState): Boolean =
      lastDenot != null && lastDenot.initial.isStatic

    /** This symbol entered into owner's scope (owner must be a class). */
    final def entered(using Ctx, CState): this.type = {
      if (this.owner.isClass) {
        this.owner.asClass.enter(this)
        if (this.is(Module)) this.owner.asClass.enter(this.moduleClass)
      }
      this
    }

    /** Enter this symbol in its class owner after given `phase`. Create a fresh
     *  denotation for its owner class if the class has not yet already one
     *  that starts being valid after `phase`.
     *  @pre  Symbol is a class member
     */
    def enteredAfter(phase: DenotTransformer)(using Ctx, CState): this.type =
      if currentPhaseId != phase.next.id then
        atPhase(phase.next)(enteredAfter(phase))
      else this.owner match {
        case owner: ClassSymbol =>
          if (owner.is(Package)) {
            denot.validFor |= InitialPeriod
            if (this.is(Module)) this.moduleClass.validFor |= InitialPeriod
          }
          else owner.ensureFreshScopeAfter(phase)
          assert(isPrivate || phase.changesMembers, i"$this entered in $owner at undeclared phase $phase")
          entered
        case _ => this
      }

    /** Remove symbol from scope of owning class */
    final def drop()(using Ctx, CState): Unit = {
      this.owner.asClass.delete(this)
      if (this.is(Module)) this.owner.asClass.delete(this.moduleClass)
    }

    /** Remove symbol from scope of owning class after given `phase`. Create a fresh
     *  denotation for its owner class if the class has not yet already one that starts being valid after `phase`.
     *  @pre  Symbol is a class member
     */
    def dropAfter(phase: DenotTransformer)(using Ctx, CState): Unit =
      if currentPhaseId != phase.next.id then
        atPhase(phase.next)(dropAfter(phase))
      else {
        assert (!this.owner.is(Package))
        this.owner.asClass.ensureFreshScopeAfter(phase)
        assert(isPrivate || phase.changesMembers, i"$this deleted in ${this.owner} at undeclared phase $phase")
        drop()
      }

    /** This symbol, if it exists, otherwise the result of evaluating `that` */
    def orElse(that: => Symbol)(using Ctx, CState): Symbol =
      if (this.exists) this else that

    /** If this symbol satisfies predicate `p` this symbol, otherwise `NoSymbol` */
    def filter(p: Symbol => Boolean): Symbol = if (p(this)) this else NoSymbol

    /** The current name of this symbol */
    final def name(using Ctx, CState): ThisName = denot.name.asInstanceOf[ThisName]

    /** The source or class file from which this class or
     *  the class containing this symbol was generated, null if not applicable.
     *  Overridden in ClassSymbol
     */
    def associatedFile(using Ctx, CState): AbstractFile =
      if (lastDenot == null) null else lastDenot.topLevelClass.associatedFile

    /** The class file from which this class was generated, null if not applicable. */
    final def binaryFile(using Ctx, CState): AbstractFile = {
      val file = associatedFile
      if (file != null && file.extension == "class") file else null
    }

    /** A trap to avoid calling x.symbol on something that is already a symbol.
     *  This would be expanded to `toDenot(x).symbol` which is guaraneteed to be
     *  the same as `x`.
     *  With the given setup, all such calls will give implicit-not found errors
     */
    final def symbol(implicit ev: DontUseSymbolOnSymbol): Nothing = unsupported("symbol")
    type DontUseSymbolOnSymbol

    final def source(using Ctx, CState): SourceFile = {
      def valid(src: SourceFile): SourceFile =
        if (src.exists && src.file.extension != "class") src
        else NoSource

      if (!denot.exists) NoSource
      else
        valid(defTree.source) match {
          case NoSource =>
            valid(denot.owner.source) match {
              case NoSource =>
                this match {
                  case cls: ClassSymbol      => valid(cls.sourceOfClass)
                  case _ if denot.is(Module) => valid(denot.moduleClass.source)
                  case _ => NoSource
                }
              case src => src
            }
          case src => src
        }
    }

    /** A symbol related to `sym` that is defined in source code.
     *
     *  @see enclosingSourceSymbols
     */
    @annotation.tailrec final def sourceSymbol(using Ctx, CState): Symbol =
      if (!denot.exists)
        this
      else if (denot.is(ModuleVal))
        this.moduleClass.sourceSymbol // The module val always has a zero-extent position
      else if (denot.is(Synthetic)) {
        val linked = denot.linkedClass
        if (linked.exists && !linked.is(Synthetic))
          linked
        else
          denot.owner.sourceSymbol
      }
      else if (denot.isPrimaryConstructor)
        denot.owner.sourceSymbol
      else this

    /** The position of this symbol, or NoSpan if the symbol was not loaded
     *  from source or from TASTY. This is always a zero-extent position.
     */
    final def span: Span = if (coord.isSpan) coord.toSpan else NoSpan

    final def sourcePos(using Ctx, CState): SourcePosition = {
      val src = source
      (if (src.exists) src else ctx.source).atSpan(span)
    }

    // ParamInfo types and methods
    def isTypeParam(using Ctx, CState): Boolean = denot.is(TypeParam)
    def paramName(using Ctx, CState): ThisName = name.asInstanceOf[ThisName]
    def paramInfo(using Ctx, CState): Type = denot.info
    def paramInfoAsSeenFrom(pre: Type)(using Ctx, CState): Type = pre.memberInfo(this)
    def paramInfoOrCompleter(using Ctx, CState): Type = denot.infoOrCompleter
    def paramVariance(using Ctx, CState): Variance = denot.variance
    def paramRef(using Ctx, CState): TypeRef = denot.typeRef

// -------- Printing --------------------------------------------------------

    /** The prefix string to be used when displaying this symbol without denotation */
    protected def prefixString: String = "Symbol"

    override def toString: String =
      if (lastDenot == null) s"Naked$prefixString#$id"
      else lastDenot.toString// + "#" + id // !!! DEBUG

    def toText(printer: Printer): Text = printer.toText(this)

    def showLocated(using Ctx, CState): String = ctx.printer.locatedText(this).show
    def showExtendedLocation(using Ctx, CState): String = ctx.printer.extendedLocationText(this).show
    def showDcl(using Ctx, CState): String = ctx.printer.dclText(this).show
    def showKind(using Ctx, CState): String = ctx.printer.kindString(this)
    def showName(using Ctx, CState): String = ctx.printer.nameString(this)
    def showFullName(using Ctx, CState): String = ctx.printer.fullNameString(this)

    override def hashCode(): Int = id // for debugging.
  }

  type TermSymbol = Symbol { type ThisName = TermName }
  type TypeSymbol = Symbol { type ThisName = TypeName }

  class ClassSymbol private[Symbols] (coord: Coord, val assocFile: AbstractFile, id: Int)
    extends Symbol(coord, id) {

    type ThisName = TypeName

    type TreeOrProvider = tpd.TreeProvider | tpd.Tree

    private var myTree: TreeOrProvider = tpd.EmptyTree

    /** If this is a top-level class and `-Yretain-trees` (or `-from-tasty`) is set.
      * Returns the TypeDef tree (possibly wrapped inside PackageDefs) for this class, otherwise EmptyTree.
      * This will force the info of the class.
      */
    def rootTree(using Ctx, CState): Tree = rootTreeContaining("")

    /** Same as `tree` but load tree only if `id == ""` or the tree might contain `id`.
     *  For Tasty trees this means consulting whether the name table defines `id`.
     *  For already loaded trees, we maintain the referenced ids in an attachment.
     */
    def rootTreeContaining(id: String)(using Ctx, CState): Tree = {
      denot.infoOrCompleter match {
        case _: NoCompleter =>
        case _ => denot.ensureCompleted()
      }
      myTree match {
        case fn: TreeProvider =>
          if (id.isEmpty || fn.mightContain(id)) {
            val tree = fn.tree
            myTree = tree
            tree
          }
          else tpd.EmptyTree
        case tree: Tree @ unchecked =>
          if (id.isEmpty || mightContain(tree, id)) tree else tpd.EmptyTree
      }
    }

    def rootTreeOrProvider: TreeOrProvider = myTree

    private[dotc] def rootTreeOrProvider_=(t: TreeOrProvider)(using Ctx, CState): Unit =
      myTree = t

    private def mightContain(tree: Tree, id: String)(using Ctx, CState): Boolean = {
      val ids = tree.getAttachment(Ids) match {
        case Some(ids) => ids
        case None =>
          val idSet = mutable.SortedSet[String]()
          tree.foreachSubTree {
            case tree: tpd.NameTree if tree.name.toTermName.isInstanceOf[SimpleName] =>
              idSet += tree.name.toString
            case _ =>
          }
          val ids = idSet.toArray
          tree.putAttachment(Ids, ids)
          ids
      }
      ids.binarySearch(id) >= 0
    }

    /** The source or class file from which this class was generated, null if not applicable. */
    override def associatedFile(using Ctx, CState): AbstractFile =
      if (assocFile != null || this.owner.is(PackageClass) || this.isEffectiveRoot) assocFile
      else super.associatedFile

    private var mySource: SourceFile = NoSource

    final def sourceOfClass(using Ctx, CState): SourceFile = {
      if (!mySource.exists && !denot.is(Package))
        // this allows sources to be added in annotations after `sourceOfClass` is first called
        mySource = {
          val file = associatedFile
          if (file != null && file.extension != "class") ctx.getSource(file)
          else {
            def sourceFromTopLevel(using Ctx, CState) =
              denot.topLevelClass.unforcedAnnotation(defn.SourceFileAnnot) match {
                case Some(sourceAnnot) => sourceAnnot.argumentConstant(0) match {
                  case Some(Constant(path: String)) => ctx.getSource(path)
                  case none => NoSource
                }
                case none => NoSource
              }
            atPhaseNoLater(flattenPhase)(sourceFromTopLevel)
          }
        }
      mySource
    }

    final def classDenot(using Ctx, CState): ClassDenotation =
      denot.asInstanceOf[ClassDenotation]

    override protected def prefixString: String = "ClassSymbol"
  }

  @sharable object NoSymbol extends Symbol(NoCoord, 0) {
    override def associatedFile(using Ctx, CState): AbstractFile = NoSource.file
    override def recomputeDenot(lastd: SymDenotation)(using Ctx, CState): SymDenotation = NoDenotation
  }

  NoDenotation // force it in order to set `denot` field of NoSymbol

  implicit class Copier[N <: Name](sym: Symbol { type ThisName = N })(using Ctx, CState) {
    /** Copy a symbol, overriding selective fields.
     *  Note that `coord` and `associatedFile` will be set from the fields in `owner`, not
     *  the fields in `sym`.
     */
    def copy(
        owner: Symbol = sym.owner,
        name: N = sym.name,
        flags: FlagSet = sym.flags,
        info: Type = sym.info,
        privateWithin: Symbol = sym.privateWithin,
        coord: Coord = NoCoord, // Can be `= owner.coord` once we boostrap
        associatedFile: AbstractFile = null // Can be `= owner.associatedFile` once we boostrap
    ): Symbol = {
      val coord1 = if (coord == NoCoord) owner.coord else coord
      val associatedFile1 = if (associatedFile == null) owner.associatedFile else associatedFile

      if (sym.isClass)
        newClassSymbol(owner, name.asTypeName, flags, _ => info, privateWithin, coord1, associatedFile1)
      else
        newSymbol(owner, name, flags, info, privateWithin, coord1)
    }
  }

  /** Makes all denotation operations available on symbols */
  implicit def toDenot(sym: Symbol)(using Ctx, CState): SymDenotation = sym.denot

  /** Makes all class denotation operations available on class symbols */
  implicit def toClassDenot(cls: ClassSymbol)(using Ctx, CState): ClassDenotation = cls.classDenot

  /** The Definitions object */
  def defn(using Ctx, CState): Definitions = ctx.definitions

  /** The current class */
  def currentClass(using Ctx, CState): ClassSymbol = ctx.owner.enclosingClass.asClass

  /* Mutable map from symbols any T */
  class MutableSymbolMap[T](private[Symbols] val value: java.util.IdentityHashMap[Symbol, T]) extends AnyVal {

    def apply(sym: Symbol): T = value.get(sym)

    def get(sym: Symbol): Option[T] = Option(value.get(sym))

    def getOrElse[U >: T](sym: Symbol, default: => U): U = {
      val v = value.get(sym)
      if (v != null) v else default
    }

    def getOrElseUpdate(sym: Symbol, op: => T): T = {
      val v = value.get(sym)
      if (v != null) v
      else {
        val v = op
        assert(v != null)
        value.put(sym, v)
        v
      }
    }

    def update(sym: Symbol, x: T): Unit = {
      assert(x != null)
      value.put(sym, x)
    }
    def put(sym: Symbol, x: T): T = {
      assert(x != null)
      value.put(sym, x)
    }

    def -=(sym: Symbol): Unit = value.remove(sym)
    def remove(sym: Symbol): Option[T] = Option(value.remove(sym))

    def contains(sym: Symbol): Boolean = value.containsKey(sym)

    def isEmpty: Boolean = value.isEmpty

    def clear(): Unit = value.clear()

    def filter(p: ((Symbol, T)) => Boolean): Map[Symbol, T] =
      value.asScala.toMap.filter(p)

    def iterator: Iterator[(Symbol, T)] = value.asScala.iterator

    def keysIterator: Iterator[Symbol] = value.keySet().asScala.iterator

    def toMap: Map[Symbol, T] = value.asScala.toMap

    override def toString: String = value.asScala.toString()
  }

  inline def newMutableSymbolMap[T]: MutableSymbolMap[T] =
    new MutableSymbolMap(new java.util.IdentityHashMap[Symbol, T]())

// ---- Factory methods for symbol creation ----------------------
//
// All symbol creations should be done via the next two methods.

  /** Create a symbol without a denotation.
   *  Note this uses a cast instead of a direct type refinement because
   *  it's debug-friendlier not to create an anonymous class here.
   */
  def newNakedSymbol[N <: Name](coord: Coord = NoCoord)(using Ctx, CState): Symbol { type ThisName = N } =
    new Symbol(coord, ctx.base.nextSymId).asInstanceOf[Symbol { type ThisName = N }]

  /** Create a class symbol without a denotation. */
  def newNakedClassSymbol(coord: Coord = NoCoord, assocFile: AbstractFile = null)(using Ctx, CState): ClassSymbol =
    new ClassSymbol(coord, assocFile, ctx.base.nextSymId)

// ---- Symbol creation methods ----------------------------------

  /** Create a symbol from its fields (info may be lazy) */
  def newSymbol[N <: Name](
      owner: Symbol,
      name: N,
      flags: FlagSet,
      info: Type,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord)(using Ctx, CState): Symbol { type ThisName = N } = {
    val sym = newNakedSymbol[N](coord)
    val denot = SymDenotation(sym, owner, name, flags, info, privateWithin)
    sym.denot = denot
    sym
  }

  /** Create a class symbol from a function producing its denotation */
  def newClassSymbolDenoting(denotFn: ClassSymbol => SymDenotation,
       coord: Coord = NoCoord, assocFile: AbstractFile = null)(using Ctx, CState): ClassSymbol = {
    val cls = newNakedClassSymbol(coord, assocFile)
    cls.denot = denotFn(cls)
    cls
  }

  /** Create a class symbol from its non-info fields and a function
   *  producing its info (the produced info may be lazy).
   */
  def newClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      infoFn: ClassSymbol => Type,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null)(using Ctx, CState): ClassSymbol
  = {
    val cls = newNakedClassSymbol(coord, assocFile)
    val denot = SymDenotation(cls, owner, name, flags, infoFn(cls), privateWithin)
    cls.denot = denot
    cls
  }

  /** Create a class symbol from its non-info fields and the fields of its info. */
  def newCompleteClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      parents: List[TypeRef],
      decls: Scope = newScope,
      selfInfo: Type = NoType,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null)(using Ctx, CState): ClassSymbol =
    newClassSymbol(
        owner, name, flags,
        ClassInfo(owner.thisType, _, parents, decls, selfInfo),
        privateWithin, coord, assocFile)

  /** Same as `newCompleteClassSymbol` except that `parents` can be a list of arbitrary
   *  types which get normalized into type refs and parameter bindings.
   */
  def newNormalizedClassSymbol(
      owner: Symbol,
      name: TypeName,
      flags: FlagSet,
      parentTypes: List[Type],
      decls: Scope = newScope,
      selfInfo: Type = NoType,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null)(using Ctx, CState): ClassSymbol = {
    def completer = new LazyType {
      def complete(denot: SymDenotation)(using Ctx, CState): Unit = {
        val cls = denot.asClass.classSymbol
        val decls = newScope
        denot.info = ClassInfo(owner.thisType, cls, parentTypes.map(_.dealias), decls)
      }
    }
    newClassSymbol(owner, name, flags, completer, privateWithin, coord, assocFile)
  }

  def newRefinedClassSymbol(coord: Coord = NoCoord)(using Ctx, CState): ClassSymbol =
    newCompleteClassSymbol(ctx.owner, tpnme.REFINE_CLASS, NonMember, parents = Nil, coord = coord)

  /** Create a module symbol with associated module class
   *  from its non-info fields and a function producing the info
   *  of the module class (this info may be lazy).
   */
  def newModuleSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet,
      clsFlags: FlagSet,
      infoFn: (TermSymbol, ClassSymbol) => Type, // typically a ModuleClassCompleterWithDecls
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null)(using Ctx, CState): TermSymbol
  = {
    val base = owner.thisType
    val module = newNakedSymbol[TermName](coord)
    val modcls = newNakedClassSymbol(coord, assocFile)
    val modclsFlags = clsFlags | ModuleClassCreationFlags
    val modclsName = name.toTypeName.adjustIfModuleClass(modclsFlags)
    val cdenot = SymDenotation(
        modcls, owner, modclsName, modclsFlags,
        infoFn(module, modcls), privateWithin)
    val mdenot = SymDenotation(
        module, owner, name, modFlags | ModuleValCreationFlags,
        if (cdenot.isCompleted) TypeRef(owner.thisType, modcls)
        else new ModuleCompleter(modcls))
    module.denot = mdenot
    modcls.denot = cdenot
    module
  }

  /** Create a module symbol with associated module class
   *  from its non-info fields and the fields of the module class info.
   *  @param flags  The combined flags of the module and the module class
   *                These are masked with RetainedModuleValFlags/RetainedModuleClassFlags.
   */
  def newCompleteModuleSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet,
      clsFlags: FlagSet,
      parents: List[TypeRef],
      decls: Scope,
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord,
      assocFile: AbstractFile = null)(using Ctx, CState): TermSymbol =
    newModuleSymbol(
        owner, name, modFlags, clsFlags,
        (module, modcls) => ClassInfo(
          owner.thisType, modcls, parents, decls, TermRef(owner.thisType, module)),
        privateWithin, coord, assocFile)

  /** Create a package symbol with associated package class
   *  from its non-info fields and a lazy type for loading the package's members.
   */
  def newPackageSymbol(
      owner: Symbol,
      name: TermName,
      infoFn: (TermSymbol, ClassSymbol) => LazyType)(using Ctx, CState): TermSymbol =
    newModuleSymbol(owner, name, PackageCreationFlags, PackageCreationFlags, infoFn)
  /** Create a package symbol with associated package class
   *  from its non-info fields its member scope.
   */
  def newCompletePackageSymbol(
      owner: Symbol,
      name: TermName,
      modFlags: FlagSet = EmptyFlags,
      clsFlags: FlagSet = EmptyFlags,
      decls: Scope = newScope)(using Ctx, CState): TermSymbol =
    newCompleteModuleSymbol(
      owner, name,
      modFlags | PackageCreationFlags, clsFlags | PackageCreationFlags,
      Nil, decls)

  /** Define a new symbol associated with a Bind or pattern wildcard and, by default, make it gadt narrowable. */
  def newPatternBoundSymbol(
      name: Name,
      info: Type,
      span: Span,
      addToGadt: Boolean = true,
      flags: FlagSet = EmptyFlags)(using Ctx, CState): Symbol = {
    val sym = newSymbol(ctx.owner, name, Case | flags, info, coord = span)
    if (addToGadt && name.isTypeName) ctx.gadt.addToConstraint(sym)
    sym
  }

  /** Create a stub symbol that will issue a missing reference error
   *  when attempted to be completed.
   */
  def newStubSymbol(owner: Symbol, name: Name, file: AbstractFile = null)(using Ctx, CState): Symbol = {
    def stubCompleter = new StubInfo()
    val normalizedOwner = if (owner.is(ModuleVal)) owner.moduleClass else owner
    typr.println(s"creating stub for ${name.show}, owner = ${normalizedOwner.denot.debugString}, file = $file")
    typr.println(s"decls = ${normalizedOwner.unforcedDecls.toList.map(_.debugString).mkString("\n  ")}") // !!! DEBUG
    //if (base.settings.debug.value) throw new Error()
    val stub = name match {
      case name: TermName =>
        newModuleSymbol(normalizedOwner, name, EmptyFlags, EmptyFlags, stubCompleter, assocFile = file)
      case name: TypeName =>
        newClassSymbol(normalizedOwner, name, EmptyFlags, stubCompleter, assocFile = file)
    }
    stub
  }

  /** Create the local template dummy of given class `cls`.
   *  In a template
   *
   *     trait T { val fld: Int; { val x: int = 2 }; val fld2 = { val y = 2; y }}
   *
   *  the owner of `x` is the local dummy of the template. The owner of the local
   *  dummy is then the class of the template itself. By contrast, the owner of `y`
   *  would be `fld2`. There is a single local dummy per template.
   */
  def newLocalDummy(cls: Symbol, coord: Coord = NoCoord)(using Ctx, CState): TermSymbol =
    newSymbol(cls, nme.localDummyName(cls), NonMember, NoType)

  /** Create an import symbol pointing back to given qualifier `expr`. */
  def newImportSymbol(owner: Symbol, expr: Tree, coord: Coord = NoCoord)(using Ctx, CState): TermSymbol =
    newImportSymbol(owner, ImportType(expr), coord = coord)

  /** Create an import symbol with given `info`. */
  def newImportSymbol(owner: Symbol, info: Type, coord: Coord)(using Ctx, CState): TermSymbol =
    newSymbol(owner, nme.IMPORT, Synthetic | NonMember, info, coord = coord)

  /** Create a class constructor symbol for given class `cls`. */
  def newConstructor(
      cls: ClassSymbol,
      flags: FlagSet,
      paramNames: List[TermName],
      paramTypes: List[Type],
      privateWithin: Symbol = NoSymbol,
      coord: Coord = NoCoord)(using Ctx, CState): TermSymbol =
    newSymbol(cls, nme.CONSTRUCTOR, flags | Method, MethodType(paramNames, paramTypes, cls.typeRef), privateWithin, coord)

  /** Create an empty default constructor symbol for given class `cls`. */
  def newDefaultConstructor(cls: ClassSymbol)(using Ctx, CState): TermSymbol =
    newConstructor(cls, EmptyFlags, Nil, Nil)

  def newLazyImplicit(info: Type, coord: Coord = NoCoord)(using Ctx, CState): TermSymbol =
    newSymbol(ctx.owner, LazyImplicitName.fresh(), EmptyFlags, info, coord = coord)

  /** Create a symbol representing a selftype declaration for class `cls`. */
  def newSelfSym(
      cls: ClassSymbol,
      name: TermName = nme.WILDCARD,
      selfInfo: Type = NoType)(using Ctx, CState): TermSymbol =
    newSymbol(cls, name, SelfSymFlags, selfInfo orElse cls.classInfo.selfType, coord = cls.coord)

  /** Create new type parameters with given owner, names, and flags.
   *  @param boundsFn  A function that, given type refs to the newly created
   *                   parameters returns a list of their bounds.
   */
  def newTypeParams(
    owner: Symbol,
    names: List[TypeName],
    flags: FlagSet,
    boundsFn: List[TypeRef] => List[Type])(using Ctx, CState): List[TypeSymbol] = {

    val tparamBuf = new mutable.ListBuffer[TypeSymbol]
    val trefBuf = new mutable.ListBuffer[TypeRef]
    for (name <- names) {
      val tparam = newSymbol(
        owner, name, flags | owner.typeParamCreationFlags, NoType, coord = owner.coord)
      tparamBuf += tparam
      trefBuf += TypeRef(owner.thisType, tparam)
    }
    val tparams = tparamBuf.toList
    val bounds = boundsFn(trefBuf.toList)
    for (tparam, bound) <- tparams.lazyZip(bounds) do
      tparam.info = bound
    tparams
  }

  /** Create a new skolem symbol. This is not the same as SkolemType, even though the
   *  motivation (create a singleton referencing to a type) is similar.
   */
  def newSkolem(tp: Type)(using Ctx, CState): TermSymbol =
    newSymbol(defn.RootClass, nme.SKOLEM, SyntheticArtifact | NonMember | Permanent, tp)

  def newErrorSymbol(owner: Symbol, name: Name, msg: Message)(using Ctx, CState): Symbol = {
    val errType = ErrorType(msg)
    newSymbol(owner, name, SyntheticArtifact,
        if (name.isTypeName) TypeAlias(errType) else errType)
  }

  /** Map given symbols, subjecting their attributes to the mappings
   *  defined in the given TreeTypeMap `ttmap`.
   *  Cross symbol references are brought over from originals to copies.
   *  Do not copy any symbols if all attributes of all symbols stay the same.
   */
  def mapSymbols(originals: List[Symbol], ttmap: TreeTypeMap, mapAlways: Boolean = false)(using Ctx, CState): List[Symbol] =
    if (originals.forall(sym =>
        (ttmap.mapType(sym.info) eq sym.info) &&
        !(ttmap.oldOwners contains sym.owner)) && !mapAlways)
      originals
    else {
      val copies: List[Symbol] = for (original <- originals) yield
        original match {
          case original: ClassSymbol =>
            newNakedClassSymbol(original.coord, original.assocFile)
          case _ =>
            newNakedSymbol[original.ThisName](original.coord)
        }
      val ttmap1 = ttmap.withSubstitution(originals, copies)
      originals.lazyZip(copies) foreach { (original, copy) =>
        val odenot = original.denot
        val oinfo = original.info match {
          case ClassInfo(pre, _, parents, decls, selfInfo) =>
            assert(original.isClass)
            ClassInfo(pre, copy.asClass, parents, decls.cloneScope, selfInfo)
          case oinfo => oinfo
        }

        val completer = new LazyType {
          def complete(denot: SymDenotation)(using Ctx, CState): Unit = {
            denot.info = oinfo // needed as otherwise we won't be able to go from Sym -> parents & etc
                               // Note that this is a hack, but hack commonly used in Dotty
                               // The same thing is done by other completers all the time
            denot.info = ttmap1.mapType(oinfo)
            denot.annotations = odenot.annotations.mapConserve(ttmap1.apply)
          }
        }

        copy.denot = odenot.copySymDenotation(
          symbol = copy,
          owner = ttmap1.mapOwner(odenot.owner),
          initFlags = odenot.flags &~ Touched,
          info = completer,
          privateWithin = ttmap1.mapOwner(odenot.privateWithin), // since this refers to outer symbols, need not include copies (from->to) in ownermap here.
          annotations = odenot.annotations)
        copy.registeredCompanion =
          copy.registeredCompanion.subst(originals, copies)
      }

      copies.foreach(_.ensureCompleted()) // avoid memory leak
      copies
    }

// ----- Locating predefined symbols ----------------------------------------

  def requiredPackage(path: PreName)(using Ctx, CState): TermSymbol = {
    val name = path.toTermName
    staticRef(name, isPackage = true).requiredSymbol("package", name)(_.is(Package)).asTerm
  }

  def requiredPackageRef(path: PreName)(using Ctx, CState): TermRef = requiredPackage(path).termRef

  def requiredClass(path: PreName)(using Ctx, CState): ClassSymbol = {
    val name = path.toTypeName
    staticRef(name).requiredSymbol("class", name)(_.isClass) match {
      case cls: ClassSymbol => cls
      case sym => defn.AnyClass
    }
  }

  def requiredClassRef(path: PreName)(using Ctx, CState): TypeRef = requiredClass(path).typeRef

  /** Get ClassSymbol if class is either defined in current compilation run
   *  or present on classpath.
   *  Returns NoSymbol otherwise. */
  def getClassIfDefined(path: PreName)(using Ctx, CState): Symbol = {
    val name = path.toTypeName
    staticRef(name, generateStubs = false)
      .requiredSymbol("class", name, generateStubs = false)(_.isClass)
  }

  /** Get a List of ClassSymbols which are either defined in current compilation
   *  run or present on classpath.
   */
  def getClassesIfDefined(paths: List[PreName])(using Ctx, CState): List[ClassSymbol] =
    paths.map(getClassIfDefined).filter(_.exists).map(_.asInstanceOf[ClassSymbol])

  /** Get ClassSymbol if package is either defined in current compilation run
   *  or present on classpath.
   *  Returns NoSymbol otherwise. */
  def getPackageClassIfDefined(path: PreName)(using Ctx, CState): Symbol = {
    val name = path.toTypeName
    staticRef(name, isPackage = true, generateStubs = false)
      .requiredSymbol("package", name, generateStubs = false)(_ is PackageClass)
  }

  def requiredModule(path: PreName)(using Ctx, CState): TermSymbol = {
    val name = path.toTermName
    staticRef(name).requiredSymbol("object", name)(_.is(Module)).asTerm
  }

  def requiredModuleRef(path: PreName)(using Ctx, CState): TermRef = requiredModule(path).termRef

  def requiredMethod(path: PreName)(using Ctx, CState): TermSymbol = {
    val name = path.toTermName
    staticRef(name).requiredSymbol("method", name)(_.is(Method)).asTerm
  }

  def requiredMethodRef(path: PreName)(using Ctx, CState): TermRef = requiredMethod(path).termRef
}