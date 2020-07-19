package dottyBench.tools
package dotc

import core._
import Contexts._
import Periods._
import Symbols._
import Types._
import Scopes._
import typer.{ImportInfo, Typer}
import Decorators._
import io.{AbstractFile, PlainFile}
import Phases.unfusedPhases

import scala.io.Codec
import util.{Set => _, _}
import reporting.Reporter
import rewrites.Rewrites
import java.io.{BufferedWriter, OutputStreamWriter}

import printing.XprintMode
import parsing.Parsers.Parser
import parsing.JavaParsers.JavaParser
import typer.ImplicitRunInfo
import collection.mutable

import dottyBench.tools.io.VirtualFile

import scala.util.control.NonFatal

/** A compiler run. Exports various methods to compile source files */
class Run(comp: Compiler, ictx: Context) extends ImplicitRunInfo with ConstraintRunInfo {

  /** Default timeout to stop looking for further implicit suggestions, in ms.
   *  This is usually for the first import suggestion; subsequent suggestions
   *  may get smaller timeouts. @see ImportSuggestions.reduceTimeBudget
   */
  private var myImportSuggestionBudget: Int =
    Int.MinValue // sentinel value; means whatever is set in command line option

  def importSuggestionBudget =
    if myImportSuggestionBudget == Int.MinValue then ictx.settings.XimportSuggestionTimeout.value
    else myImportSuggestionBudget

  def importSuggestionBudget_=(x: Int) =
    myImportSuggestionBudget = x

  /** If this variable is set to `true`, some core typer operations will
   *  return immediately. Currently these early abort operations are
   *  `Typer.typed` and `Implicits.typedImplicit`.
   */
  @volatile var isCancelled = false

  /** Produces the following contexts, from outermost to innermost
   *
   *    bootStrap:   A context with next available runId and a scope consisting of
   *                 the RootPackage _root_
   *    start        A context with RootClass as owner and the necessary initializations
   *                 for type checking.
   *    imports      For each element of RootImports, an import context
   */
  protected def rootContext(using ctx: Context): Context = {
    ctx.initialize()
    ctx.base.setPhasePlan(comp.phases)
    val rootScope = new MutableScope
    val bootstrap = ctx.fresh
      .setPeriod(Period(comp.nextRunId, FirstPhaseId))
      .setScope(rootScope)
    rootScope.enter(ctx.definitions.RootPackage)(using bootstrap, bootstrap.cstate)
    val start = bootstrap.fresh
      .setOwner(defn.RootClass)
      .setTyper(new Typer)
      .addMode(Mode.ImplicitsEnabled)
      .setTyperState(ctx.typerState.fresh(ctx.reporter))
    ctx.initialize()(using start) // re-initialize the base context with start
    atPeriod(ctx.period) {
      def addImport(ctx: Context, rootRef: ImportInfo.RootRef) =
        ctx.fresh.setImportInfo(ImportInfo.rootImport(rootRef))
      defn.RootImportFns.foldLeft(start.setRun(this))(addImport)
    }
  }

  private var compiling = false

  private var myCtx = rootContext(using ictx)

  /** The context created for this run */
  given runContext[Dummy_so_its_a_def] as Context = myCtx
  given runCState[Dummy_so_its_a_def] as CState =
    if myCtx == null then Nowhere else myCtx.cstate

  assert(currentRunId(using runContext) <= Periods.MaxPossibleRunId)

  private var myUnits: List[CompilationUnit] = _
  private var myUnitsCached: List[CompilationUnit] = _
  private var myFiles: Set[AbstractFile] = _

  /** The compilation units currently being compiled, this may return different
    *  results over time.
    */
  def units: List[CompilationUnit] = myUnits

  var suspendedUnits: mutable.ListBuffer[CompilationUnit] = mutable.ListBuffer()

  private def units_=(us: List[CompilationUnit]): Unit =
    myUnits = us

  /** The files currently being compiled (active or suspended).
   *  This may return different results over time.
   *  These files do not have to be source files since it's possible to compile
   *  from TASTY.
   */
  def files: Set[AbstractFile] = {
    if (myUnits ne myUnitsCached) {
      myUnitsCached = myUnits
      myFiles = (myUnits ++ suspendedUnits).map(_.source.file).toSet
    }
    myFiles
  }

  /** The source files of all late entered symbols, as a set */
  private var lateFiles = mutable.Set[AbstractFile]()

  /** Actions that need to be performed at the end of the current compilation run */
  private var finalizeActions = mutable.ListBuffer[() => Unit]()

  def compile(fileNames: List[String]): Unit = try {
    val sources = fileNames.map(runContext.getSource(_))
    compileSources(sources)
  }
  catch {
    case NonFatal(ex) =>
      if units != null then report.echo(i"exception occurred while compiling $units%, %")
      else report.echo(s"exception occurred while compiling ${fileNames.mkString(", ")}")
      throw ex
  }

  /** TODO: There's a fundamental design problem here: We assemble phases using `squash`
   *  when we first build the compiler. But we modify them with -Yskip, -Ystop
   *  on each run. That modification needs to either transform the tree structure,
   *  or we need to assemble phases on each run, and take -Yskip, -Ystop into
   *  account. I think the latter would be preferable.
   */
  def compileSources(sources: List[SourceFile]): Unit =
    if (sources forall (_.exists)) {
      units = sources.map(CompilationUnit(_))
      compileUnits()
    }

  def compileUnits(us: List[CompilationUnit]): Unit = {
    units = us
    compileUnits()
  }

  def compileUnits(us: List[CompilationUnit], ctx: Context): Unit = {
    units = us
    compileUnits()(using ctx)
  }

  private def compileUnits()(using Ctx, CState) = Stats.maybeMonitored {
    if (!ctx.mode.is(Mode.Interactive)) // IDEs might have multi-threaded access, accesses are synchronized
      ctx.base.checkSingleThreaded()

    compiling = true

    // If testing pickler, make sure to stop after pickling phase:
    val stopAfter =
      if (ctx.settings.YtestPickler.value) List("pickler")
      else ctx.settings.YstopAfter.value

    val pluginPlan = ctx.base.addPluginPhases(ctx.base.phasePlan)
    val phases = ctx.base.fusePhases(pluginPlan,
      ctx.settings.Yskip.value, ctx.settings.YstopBefore.value, stopAfter, ctx.settings.Ycheck.value)
    ctx.base.usePhases(phases)

    def runPhases(using c: Context) = {
      given CState = c.cstate
      var lastPrintedTree: PrintedTree = NoPrintedTree

      for (phase <- ctx.base.allPhases)
        if (phase.isRunnable)
          Stats.trackTime(s"$phase ms ") {
            val start = System.currentTimeMillis
            units = phase.runOn(units)
            if (ctx.settings.Xprint.value.containsPhase(phase))
              for (unit <- units)
                lastPrintedTree = atPhase(phase.next) {
                  printTree(lastPrintedTree)(using ctx.fresh.setCompilationUnit(unit))
                }
            report.informTime(s"$phase ", start)
            Stats.record(s"total trees at end of $phase", ast.Trees.ntrees)
            for (unit <- units)
              Stats.record(s"retained typed trees at end of $phase", unit.tpdTree.treeSize)
          }
    }

    val runCtx = currentContext.fresh
    unfusedPhases.foreach(_.initContext(runCtx))
    runPhases(using runCtx)
    if (!runCtx.reporter.hasErrors) Rewrites.writeBack()
    while (finalizeActions.nonEmpty) {
      val action = finalizeActions.remove(0)
      action()
    }
    compiling = false
  }

  /** Enter top-level definitions of classes and objects contain in Scala source file `file`.
   *  The newly added symbols replace any previously entered symbols.
   *  If `typeCheck = true`, also run typer on the compilation unit, and set
   *  `rootTreeOrProvider`.
   */
  def lateCompile(file: AbstractFile, typeCheck: Boolean)(using Ctx, CState): Unit =
    if (!files.contains(file) && !lateFiles.contains(file)) {
      lateFiles += file
      val unit = CompilationUnit(ctx.getSource(file.path))
      def process()(using Ctx, CState) = {
        unit.untpdTree =
          if (unit.isJava) new JavaParser(unit.source).parse()
          else new Parser(unit.source).parse()
        ctx.typer.lateEnter(unit.untpdTree)
        def processUnit() = {
          unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
          val phase = new transform.SetRootTree()
          phase.run
        }
        if (typeCheck)
          if (compiling) finalizeActions += (() => processUnit()) else processUnit()
      }
      process()(using runContext.fresh.setCompilationUnit(unit))
    }

  private sealed trait PrintedTree
  private /*final*/ case class SomePrintedTree(phase: String, tree: String) extends PrintedTree
  private object NoPrintedTree extends PrintedTree

  private def printTree(last: PrintedTree)(using Ctx, CState): PrintedTree = {
    val unit = ctx.compilationUnit
    val prevPhase = currentPhase.prev // can be a mini-phase
    val squashedPhase = ctx.base.squashed(prevPhase)
    val treeString = unit.tpdTree.show(using ctx.withProperty(XprintMode, Some(())))

    report.echo(s"result of $unit after $squashedPhase:")

    last match {
      case SomePrintedTree(phase, lastTreeSting) if lastTreeSting != treeString =>
        val msg =
          if (!ctx.settings.XprintDiff.value && !ctx.settings.XprintDiffDel.value) treeString
          else DiffUtil.mkColoredCodeDiff(treeString, lastTreeSting, ctx.settings.XprintDiffDel.value)
        report.echo(msg)
        SomePrintedTree(squashedPhase.toString, treeString)

      case SomePrintedTree(phase, lastTreeSting) =>
        report.echo("  Unchanged since " + phase)
        last

      case NoPrintedTree =>
        report.echo(treeString)
        SomePrintedTree(squashedPhase.toString, treeString)
    }
  }

  def compileFromStrings(scalaSources: List[String], javaSources: List[String] = Nil): Unit = {
    def sourceFile(source: String, isJava: Boolean): SourceFile = {
      val uuid = java.util.UUID.randomUUID().toString
      val ext = if (isJava) ".java" else ".scala"
      val virtualFile = new VirtualFile(s"compileFromString-$uuid.$ext")
      val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8")) // buffering is still advised by javadoc
      writer.write(source)
      writer.close()
      new SourceFile(virtualFile, Codec.UTF8)
    }
    val sources =
      scalaSources.map(sourceFile(_, isJava = false)) ++
       javaSources.map(sourceFile(_, isJava = true))

    compileSources(sources)
  }

  /** Print summary; return # of errors encountered */
  def printSummary(): Unit = {
    printMaxConstraint()
    val r = runContext.reporter
    r.printSummary
  }

  override def reset(): Unit = {
    super[ImplicitRunInfo].reset()
    super[ConstraintRunInfo].reset()
    myCtx = null
    myUnits = null
    myUnitsCached = null
  }
}