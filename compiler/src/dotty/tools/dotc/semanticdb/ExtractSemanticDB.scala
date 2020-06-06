package dotty.tools
package dotc
package semanticdb

import core._
import Phases._
import ast.tpd._
import ast.Trees.mods
import Contexts._
import Symbols._
import Flags._
import Names.Name
import StdNames.nme
import util.Spans.Span
import util.{SourceFile, SourcePosition}
import scala.jdk.CollectionConverters._
import collection.mutable
import java.nio.file.Paths

import PartialFunction.condOpt

import ast.untpd.{given _}
import NameOps.{given _}

import scala.annotation.{ threadUnsafe => tu, tailrec }

/** Extract symbol references and uses to semanticdb files.
 *  See https://scalameta.org/docs/semanticdb/specification.html#symbol-1
 *  for a description of the format.
 *  TODO: Also extract type information
 */
class ExtractSemanticDB extends Phase:
  import Scala3.{_, given _}
  import Symbols.{given _}

  override val phaseName: String = ExtractSemanticDB.name

  override def isRunnable(implicit ctx: Context) =
    super.isRunnable && ctx.settings.Ysemanticdb.value

  // Check not needed since it does not transform trees
  override def isCheckable: Boolean = false

  override def run(implicit ctx: Context): Unit =
    val unit = ctx.compilationUnit
    val extract = Extractor()
    extract.traverse(unit.tpdTree)
    ExtractSemanticDB.write(unit.source, extract.occurrences.toList, extract.symbolInfos.toList)

  /** Extractor of symbol occurrences from trees */
  class Extractor extends TreeTraverser:

    private var nextLocalIdx: Int = 0

    /** The index of a local symbol */
    private val locals = mutable.HashMap[Symbol, Int]()

    /** The bodies of synthetic locals */
    private val localBodies = mutable.HashMap[Symbol, Tree]()

    /** The local symbol(s) starting at given offset */
    private val symsAtOffset = new mutable.HashMap[Int, Set[Symbol]]():
      override def default(key: Int) = Set[Symbol]()

    /** The extracted symbol occurrences */
    val occurrences = new mutable.ListBuffer[SymbolOccurrence]()

    /** The extracted symbol infos */
    val symbolInfos = new mutable.ListBuffer[SymbolInformation]()

    /** A cache of localN names */
    val localNames = new mutable.HashSet[String]()

    /** The symbol occurrences generated so far, as a set */
    private val generated = new mutable.HashSet[SymbolOccurrence]

    /** Definitions of this symbol should be excluded from semanticdb */
    private def excludeDef(sym: Symbol)(using Context): Boolean =
      !sym.exists
      || sym.isLocalDummy
      || sym.is(Synthetic)
      || sym.isSetter
      || excludeDefOrUse(sym)

    private def excludeDefOrUse(sym: Symbol)(using Context): Boolean =
      !sym.exists
      || sym.name.is(NameKinds.DefaultGetterName)
      || sym.isConstructor && (sym.owner.is(ModuleClass) || !sym.isGlobal)
      || excludeSymbol(sym)

    private def excludeSymbol(sym: Symbol)(using Context): Boolean =
      !sym.exists
      || sym.name.isWildcard
      || excludeQual(sym)

    private def excludeQual(sym: Symbol)(using Context): Boolean =
      !sym.exists
      || sym.isAnonymousFunction
      || sym.isAnonymousModuleVal
      || sym.name.isEmptyNumbered

    private def excludeChildren(sym: Symbol)(using Context): Boolean =
      !sym.exists
      || sym.isAllOf(HigherKinded | Param)

    /** Uses of this symbol where the reference has given span should be excluded from semanticdb */
    private def excludeUse(qualifier: Option[Symbol], sym: Symbol)(using Context): Boolean =
      !sym.exists
      || excludeDefOrUse(sym)
      || sym.isConstructor && sym.owner.isAnnotation
      || sym == defn.Any_typeCast
      || sym.owner == defn.OpsPackageClass
      || qualifier.exists(excludeQual)

    private def traverseAnnotsOfDefinition(sym: Symbol)(using Context): Unit =
      for annot <- sym.annotations do
        if annot.tree.span.exists
        && annot.tree.span.hasLength
          annot.tree match
            case tree: Typed => () // hack for inline code
            case tree        => traverse(tree)

    override def traverse(tree: Tree)(using Context): Unit =

      tree match
        case tree: DefTree if tree.symbol.exists =>
          traverseAnnotsOfDefinition(tree.symbol)
        case _ =>
          ()

      tree match
        case tree: PackageDef =>
          if !excludeDef(tree.pid.symbol)
          && tree.pid.span.hasLength
            tree.pid match
            case tree: Select =>
              registerDefinition(tree.symbol, selectSpan(tree), Set.empty, tree.source)
              traverse(tree.qualifier)
            case tree => registerDefinition(tree.symbol, tree.span, Set.empty, tree.source)
          tree.stats.foreach(traverse)
        case tree: NamedDefTree =>
          if tree.symbol.isAllOf(ModuleValCreationFlags)
            return
          if !excludeDef(tree.symbol)
          && tree.span.hasLength
            registerDefinition(tree.symbol, tree.adjustedNameSpan, symbolKinds(tree), tree.source)
            val privateWithin = tree.symbol.privateWithin
            if privateWithin.exists
              registerUseGuarded(None, privateWithin, spanOfSymbol(privateWithin, tree.span, tree.source), tree.source)
          else if !excludeSymbol(tree.symbol)
            registerSymbol(tree.symbol, symbolName(tree.symbol), symbolKinds(tree))
          tree match
          case tree: ValDef
          if tree.symbol.isAllOf(EnumValue) =>
            tree.rhs match
            case Block(TypeDef(_, template: Template) :: _, _) => // simple case with specialised extends clause
              template.parents.foreach(traverse)
            case _ => // calls $new
          case tree: ValDef
          if tree.symbol.isSelfSym =>
            if tree.tpt.span.hasLength
              traverse(tree.tpt)
          case tree: DefDef
          if tree.symbol.isConstructor => // ignore typeparams for secondary ctors
            tree.vparamss.foreach(_.foreach(traverse))
            traverse(tree.rhs)
          case tree: (DefDef | ValDef)
          if tree.symbol.isSyntheticWithIdent =>
            tree match
              case tree: DefDef =>
                tree.tparams.foreach(tparam => registerSymbolSimple(tparam.symbol))
                tree.vparamss.foreach(_.foreach(vparam => registerSymbolSimple(vparam.symbol)))
              case _ =>
            if !tree.symbol.isGlobal
              localBodies(tree.symbol) = tree.rhs
            // ignore rhs
          case PatternValDef(pat, rhs) =>
            traverse(rhs)
            PatternValDef.collectPats(pat).foreach(traverse)
          case tree =>
            if !excludeChildren(tree.symbol)
              traverseChildren(tree)
        case tree: Template =>
          val ctorSym = tree.constr.symbol
          if !excludeDef(ctorSym)
            traverseAnnotsOfDefinition(ctorSym)
            registerDefinition(ctorSym, tree.constr.span, Set.empty, tree.source)
            ctorParams(tree.constr.vparamss, tree.body)
          for parent <- tree.parentsOrDerived if parent.span.hasLength do
            traverse(parent)
          val selfSpan = tree.self.span
          if selfSpan.exists && selfSpan.hasLength then
            traverse(tree.self)
          if tree.symbol.owner.is(Enum, butNot=Case)
            tree.body.foreachUntilImport(traverse).foreach(traverse) // the first import statement
          else
            tree.body.foreach(traverse)
        case tree: Apply =>
          @tu lazy val genParamSymbol: Name => String = funParamSymbol(tree.fun.symbol)
          traverse(tree.fun)
          for arg <- tree.args do
            arg match
              case tree @ NamedArg(name, arg) =>
                registerUse(genParamSymbol(name), tree.span.startPos.withEnd(tree.span.start + name.toString.length), tree.source)
                traverse(localBodies.get(arg.symbol).getOrElse(arg))
              case _ => traverse(arg)
        case tree: Assign =>
          val qualSym = condOpt(tree.lhs) { case Select(qual, _) if qual.symbol.exists => qual.symbol }
          if !excludeUse(qualSym, tree.lhs.symbol)
            val lhs = tree.lhs.symbol
            val setter = lhs.matchingSetter.orElse(lhs)
            tree.lhs match
              case tree: Select => registerUse(setter, selectSpan(tree), tree.source)
              case tree         => registerUse(setter, tree.span, tree.source)
            traverseChildren(tree.lhs)
          traverse(tree.rhs)
        case tree: Ident =>
          if tree.name != nme.WILDCARD then
            val sym = tree.symbol.adjustIfCtorTyparam
            registerUseGuarded(None, sym, tree.span, tree.source)
        case tree: Select =>
          val qual = tree.qualifier
          val qualSpan = qual.span
          val sym = tree.symbol.adjustIfCtorTyparam
          registerUseGuarded(qual.symbol.ifExists, sym, selectSpan(tree), tree.source)
          if qualSpan.exists && qualSpan.hasLength then
            traverse(qual)
        case tree: Import =>
          if tree.span.exists && tree.span.hasLength then
            for sel <- tree.selectors do
              val imported = sel.imported.name
              if imported != nme.WILDCARD then
                for alt <- tree.expr.tpe.member(imported).alternatives do
                  registerUseGuarded(None, alt.symbol, sel.imported.span, tree.source)
                  if (alt.symbol.companionClass.exists)
                    registerUseGuarded(None, alt.symbol.companionClass, sel.imported.span, tree.source)
            traverseChildren(tree)
        case tree: Inlined =>
          traverse(tree.call)
        case _ =>
          traverseChildren(tree)

    end traverse

    private def funParamSymbol(funSym: Symbol)(using Context): Name => String =
      if funSym.isGlobal then
        val funSymbol = symbolName(funSym)
        name => s"$funSymbol($name)"
      else
        name => locals.keys.find(local => local.isTerm && local.owner == funSym && local.name == name)
                      .fold("<?>")(Symbols.LocalPrefix + _)

    private object PatternValDef:

      def unapply(tree: ValDef)(using Context): Option[(Tree, Tree)] = tree.rhs match

        case Match(Typed(selected: Tree, tpt: TypeTree), CaseDef(pat: Tree, _, _) :: Nil)
        if tpt.span.exists && !tpt.span.hasLength && tpt.tpe.isAnnotatedByUnchecked =>
          Some((pat, selected))

        case _ => None

      private inline def (tpe: Types.Type) isAnnotatedByUnchecked(using Context) = tpe match
        case Types.AnnotatedType(_, annot) => annot.symbol == defn.UncheckedAnnot
        case _                             => false

      def collectPats(pat: Tree): List[Tree] =

        @tailrec
        def impl(acc: List[Tree], pats: List[Tree]): List[Tree] = pats match

          case pat::pats => pat match
            case Typed(UnApply(fun: Tree, _, args), tpt: Tree) => impl(fun::tpt::acc, args:::pats)
            case Typed(obj: Ident, tpt: Tree)                  => impl(obj::tpt::acc, pats)
            case UnApply(fun: Tree, _, args)                   => impl(fun::acc,      args:::pats)
            case obj: Ident                                    => impl(obj::acc,      pats)
            case _                                             => impl(acc,           pats)

          case Nil => acc

        impl(Nil, pat::Nil)

    end PatternValDef

    private def (tree: NamedDefTree) adjustedNameSpan(using Context): Span =
      if tree.span.exists && tree.name.isAnonymousFunctionName || tree.name.isAnonymousClassName
        Span(tree.span.point)
      else
        tree.nameSpan

    /** Add semanticdb name of the given symbol to string builder */
    private def addSymName(b: StringBuilder, sym: Symbol)(using Context): Unit =

      def addName(name: Name) =
        val str = name.toString.unescapeUnicode
        if str.isJavaIdent then b append str
        else b append '`' append str append '`'

      def addOwner(owner: Symbol): Unit =
        if !owner.isRoot then addSymName(b, owner)

      def addOverloadIdx(sym: Symbol): Unit =
        val decls =
          val decls0 = sym.owner.info.decls.lookupAll(sym.name)
          if sym.owner.isAllOf(JavaModule)
            decls0 ++ sym.owner.companionClass.info.decls.lookupAll(sym.name)
          else
            decls0
        end decls
        val alts = decls.filter(_.isOneOf(Method | Mutable)).toList.reverse
        def find(filter: Symbol => Boolean) = alts match
          case notSym :: rest if !filter(notSym) =>
            val idx = rest.indexWhere(filter).ensuring(_ >= 0)
            b.append('+').append(idx + 1)
          case _ =>
        end find
        val sig = sym.signature
        find(_.signature == sig)

      def addDescriptor(sym: Symbol): Unit =
        if sym.is(ModuleClass) then
          addDescriptor(sym.sourceModule)
        else if sym.is(TypeParam) then
          b.append('['); addName(sym.name); b.append(']')
        else if sym.is(Param) then
          b.append('('); addName(sym.name); b.append(')')
        else if sym.isRoot then
          b.append(Symbols.RootPackage)
        else if sym.isEmptyPackage then
          b.append(Symbols.EmptyPackage)
        else if (sym.isScala2PackageObject) then
          b.append(Symbols.PackageObjectDescriptor)
        else
          addName(sym.name)
          if sym.is(Package) then b.append('/')
          else if sym.isType || sym.isAllOf(JavaModule) then b.append('#')
          else if sym.isOneOf(Method | Mutable)
          && (!sym.is(StableRealizable) || sym.isConstructor) then
            b.append('('); addOverloadIdx(sym); b.append(").")
          else b.append('.')

      /** The index of local symbol `sym`. Symbols with the same name and
       *  the same starting position have the same index.
       */
      def localIdx(sym: Symbol)(using Context): Int =
        val startPos =
          assert(sym.span.exists, s"$sym should have a span")
          sym.span.start
        @tailrec
        def computeLocalIdx(sym: Symbol): Int = locals get sym match
          case Some(idx) => idx
          case None      => symsAtOffset(startPos).find(_.name == sym.name) match
            case Some(other) => computeLocalIdx(other)
            case None =>
              val idx = nextLocalIdx
              nextLocalIdx += 1
              locals(sym) = idx
              symsAtOffset(startPos) += sym
              idx
        end computeLocalIdx
        computeLocalIdx(sym)
      end localIdx

      if sym.exists then
        if sym.isGlobal then
          addOwner(sym.owner); addDescriptor(sym)
        else
          b.append(Symbols.LocalPrefix).append(localIdx(sym))

    end addSymName

    /** The semanticdb name of the given symbol */
    private def symbolName(sym: Symbol)(using Context): String =
      val b = StringBuilder(20)
      addSymName(b, sym)
      b.toString

    private def range(span: Span, treeSource: SourceFile)(using Context): Option[Range] =
      def lineCol(offset: Int) = (treeSource.offsetToLine(offset), treeSource.column(offset))
      val (startLine, startCol) = lineCol(span.start)
      val (endLine, endCol) = lineCol(span.end)
      Some(Range(startLine, startCol, endLine, endCol))

    private def symbolKind(sym: Symbol, symkinds: Set[SymbolKind])(using Context): SymbolInformation.Kind =
      if sym.isTypeParam
        SymbolInformation.Kind.TYPE_PARAMETER
      else if sym.is(TermParam)
        SymbolInformation.Kind.PARAMETER
      else if sym.isTerm && sym.owner.isTerm
        SymbolInformation.Kind.LOCAL
      else if sym.isInlineMethod || sym.is(Macro)
        SymbolInformation.Kind.MACRO
      else if sym.isConstructor
        SymbolInformation.Kind.CONSTRUCTOR
      else if sym.isSelfSym
        SymbolInformation.Kind.SELF_PARAMETER
      else if sym.isOneOf(Method) || symkinds.exists(_.isVarOrVal)
        SymbolInformation.Kind.METHOD
      else if sym.isPackageObject
        SymbolInformation.Kind.PACKAGE_OBJECT
      else if sym.is(Module)
        SymbolInformation.Kind.OBJECT
      else if sym.is(Package)
        SymbolInformation.Kind.PACKAGE
      else if sym.isAllOf(JavaInterface)
        SymbolInformation.Kind.INTERFACE
      else if sym.is(Trait)
        SymbolInformation.Kind.TRAIT
      else if sym.isClass
        SymbolInformation.Kind.CLASS
      else if sym.isType
        SymbolInformation.Kind.TYPE
      else if sym.is(ParamAccessor)
        SymbolInformation.Kind.FIELD
      else
        SymbolInformation.Kind.UNKNOWN_KIND

    private def symbolProps(sym: Symbol, symkinds: Set[SymbolKind])(using Context): Int =
      if sym.is(ModuleClass)
        return symbolProps(sym.sourceModule, symkinds)
      var props = 0
      if sym.isPrimaryConstructor
        props |= SymbolInformation.Property.PRIMARY.value
      if sym.is(Abstract) || symkinds.contains(SymbolKind.Abstract)
        props |= SymbolInformation.Property.ABSTRACT.value
      if sym.is(Final)
        props |= SymbolInformation.Property.FINAL.value
      if sym.is(Sealed)
        props |= SymbolInformation.Property.SEALED.value
      if sym.isOneOf(GivenOrImplicit)
        props |= SymbolInformation.Property.IMPLICIT.value
      if sym.is(Lazy, butNot=Module)
        props |= SymbolInformation.Property.LAZY.value
      if sym.isAllOf(Case | Module) || sym.is(CaseClass) || sym.isAllOf(EnumCase)
        props |= SymbolInformation.Property.CASE.value
      if sym.is(Covariant)
        props |= SymbolInformation.Property.COVARIANT.value
      if sym.is(Contravariant)
        props |= SymbolInformation.Property.CONTRAVARIANT.value
      if sym.isAllOf(DefaultMethod | JavaDefined) || sym.is(Accessor) && sym.name.is(NameKinds.DefaultGetterName)
        props |= SymbolInformation.Property.DEFAULT.value
      if symkinds.exists(_.isVal)
        props |= SymbolInformation.Property.VAL.value
      if symkinds.exists(_.isVar)
        props |= SymbolInformation.Property.VAR.value
      if sym.is(JavaStatic)
        props |= SymbolInformation.Property.STATIC.value
      if sym.is(Enum)
        props |= SymbolInformation.Property.ENUM.value
      props

    private def symbolInfo(sym: Symbol, symbolName: String, symkinds: Set[SymbolKind])(using Context): SymbolInformation =
      SymbolInformation(
        symbol = symbolName,
        language = Language.SCALA,
        kind = symbolKind(sym, symkinds),
        properties = symbolProps(sym, symkinds),
        displayName = Symbols.displaySymbol(sym)
      )

    private def registerSymbol(sym: Symbol, symbolName: String, symkinds: Set[SymbolKind])(using Context): Unit =
      val isLocal = symbolName.isLocal
      if !isLocal || !localNames.contains(symbolName)
        if isLocal
          localNames += symbolName
        symbolInfos += symbolInfo(sym, symbolName, symkinds)

    private def registerSymbolSimple(sym: Symbol)(using Context): Unit =
      registerSymbol(sym, symbolName(sym), Set.empty)

    private def registerOccurrence(symbol: String, span: Span, role: SymbolOccurrence.Role, treeSource: SourceFile)(using Context): Unit =
      val occ = SymbolOccurrence(symbol, range(span, treeSource), role)
      if !generated.contains(occ) && occ.symbol.nonEmpty then
        occurrences += occ
        generated += occ

    private def registerUseGuarded(qualSym: Option[Symbol], sym: Symbol, span: Span, treeSource: SourceFile)(using Context) =
      if !excludeUse(qualSym, sym) then
        registerUse(sym, span, treeSource)

    private def registerUse(sym: Symbol, span: Span, treeSource: SourceFile)(using Context): Unit =
      registerUse(symbolName(sym), span, treeSource)

    private def registerUse(symbol: String, span: Span, treeSource: SourceFile)(using Context): Unit =
      registerOccurrence(symbol, span, SymbolOccurrence.Role.REFERENCE, treeSource)

    private def registerDefinition(sym: Symbol, span: Span, symkinds: Set[SymbolKind], treeSource: SourceFile)(using Context) =
      val symbol = symbolName(sym)
      registerOccurrence(symbol, span, SymbolOccurrence.Role.DEFINITION, treeSource)
      if !sym.is(Package)
        registerSymbol(sym, symbol, symkinds)

    private def spanOfSymbol(sym: Symbol, span: Span, treeSource: SourceFile)(using Context): Span =
      val contents = if treeSource.exists then treeSource.content() else Array.empty[Char]
      val idx = contents.indexOfSlice(sym.name.show, span.start)
      val start = if idx >= 0 then idx else span.start
      Span(start, start + sym.name.show.length, start)

    private inline def (list: List[List[ValDef]]) isSingleArg = list match
      case (_::Nil)::Nil => true
      case _             => false

    private def (tree: DefDef) isSetterDef(using Context): Boolean =
      tree.name.isSetterName && tree.mods.is(Accessor) && tree.vparamss.isSingleArg

    private def findGetters(ctorParams: Set[Names.TermName], body: List[Tree])(using Context): Map[Names.TermName, ValDef] =
      if ctorParams.isEmpty || body.isEmpty then
        Map.empty
      else
        body.collect({
          case tree: ValDef
          if ctorParams.contains(tree.name)
          && !tree.symbol.isPrivate =>
            tree.name -> tree
        }).toMap
    end findGetters

    private def selectSpan(tree: Select) =
      val end = tree.span.end
      val limit = tree.qualifier.span.end
      val start =
        if limit < end then
          val len = tree.name.toString.length
          if tree.source.content()(end - 1) == '`' then end - len - 2 else end - len
        else limit
      Span(start max limit, end)

    private extension on (span: Span):
      def hasLength: Boolean = span.start != span.end
      def zeroLength: Boolean = span.start == span.end

    /**Consume head while not an import statement.
     * Returns the rest of the list after the first import, or else the empty list
     */
    @tailrec
    private def (body: List[Tree]) foreachUntilImport(op: Tree => Unit): List[Tree] = body match
      case ((_: Import) :: rest) => rest
      case stat :: rest =>
        op(stat)
        rest.foreachUntilImport(op)
      case Nil => Nil

    private def (sym: Symbol) adjustIfCtorTyparam(using Context) =
      if sym.isType && sym.owner.exists && sym.owner.isConstructor
        matchingMemberType(sym, sym.owner.owner)
      else
        sym

    private inline def matchingMemberType(ctorTypeParam: Symbol, classSym: Symbol)(using Context) =
      classSym.info.member(ctorTypeParam.name).symbol

    /**Necessary because not all of the eventual flags are propagated from the Tree to the symbol yet.
     */
    private def symbolKinds(tree: NamedDefTree)(using Context): Set[SymbolKind] =
      if tree.symbol.isSelfSym
        Set.empty
      else
        val symkinds = mutable.HashSet.empty[SymbolKind]
        tree match
        case tree: ValDef =>
          if !tree.symbol.is(Param)
            symkinds += (if tree.mods is Mutable then SymbolKind.Var else SymbolKind.Val)
          if tree.rhs.isEmpty && !tree.symbol.isOneOf(TermParam | CaseAccessor | ParamAccessor)
            symkinds += SymbolKind.Abstract
        case tree: DefDef =>
          if tree.isSetterDef
            symkinds += SymbolKind.Setter
          else if tree.rhs.isEmpty
            symkinds += SymbolKind.Abstract
        case tree: Bind =>
          symkinds += SymbolKind.Val
        case _ =>
        symkinds.toSet

    private def ctorParams(
      vparamss: List[List[ValDef]], body: List[Tree])(using Context): Unit =
      @tu lazy val getters = findGetters(vparamss.flatMap(_.map(_.name)).toSet, body)
      for
        vparams <- vparamss
        vparam  <- vparams
      do
        if !excludeSymbol(vparam.symbol)
          traverseAnnotsOfDefinition(vparam.symbol)
          val symkinds =
            getters.get(vparam.name).fold(SymbolKind.emptySet)(getter =>
              if getter.mods.is(Mutable) then SymbolKind.VarSet else SymbolKind.ValSet)
          registerSymbol(vparam.symbol, symbolName(vparam.symbol), symkinds)
        traverse(vparam.tpt)

object ExtractSemanticDB:
  import java.nio.file.Path
  import scala.collection.JavaConverters._
  import java.nio.file.Files

  val name: String = "extractSemanticDB"

  def write(source: SourceFile, occurrences: List[SymbolOccurrence], symbolInfos: List[SymbolInformation])(using Context): Unit =
    def absolutePath(path: Path): Path = path.toAbsolutePath.normalize
    def commonPrefix[T](z: T)(i1: Iterable[T], i2: Iterable[T])(app: (T, T) => T): T =
      (i1 lazyZip i2).takeWhile(p => p(0) == p(1)).map(_(0)).foldLeft(z)(app)
    val sourcePath = absolutePath(source.file.jpath)
    val sourceRoot =
      // Here if `sourceRoot` and `sourcePath` do not share a common prefix then `relPath` will not be normalised,
      // containing ../.. etc, which is problematic when appending to `/META-INF/semanticdb/` and will not be accepted
      // by Files.createDirectories on JDK 11.
      val sourceRoot0 = absolutePath(Paths.get(ctx.settings.sourceroot.value))
      commonPrefix(sourcePath.getRoot)(sourcePath.asScala, sourceRoot0.asScala)(_ resolve _)
    val semanticdbTarget =
      val semanticdbTargetSetting = ctx.settings.semanticdbTarget.value
      absolutePath(
        if semanticdbTargetSetting.isEmpty then ctx.settings.outputDir.value.jpath
        else Paths.get(semanticdbTargetSetting)
      )
    val relPath = sourceRoot.relativize(sourcePath)
    val outpath = semanticdbTarget
      .resolve("META-INF")
      .resolve("semanticdb")
      .resolve(relPath)
      .resolveSibling(sourcePath.getFileName().toString() + ".semanticdb")
    Files.createDirectories(outpath.getParent())
    val doc: TextDocument = TextDocument(
      schema = Schema.SEMANTICDB4,
      language = Language.SCALA,
      uri = relPath.toString,
      text = "",
      md5 = internal.MD5.compute(String(source.content)),
      symbols = symbolInfos,
      occurrences = occurrences
    )
    val docs = TextDocuments(List(doc))
    val out = Files.newOutputStream(outpath)
    try
      val stream = internal.SemanticdbOutputStream.newInstance(out)
      docs.writeTo(stream)
      stream.flush()
    finally
      out.close()
