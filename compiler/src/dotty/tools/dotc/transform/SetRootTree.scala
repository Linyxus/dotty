package dotty.tools.dotc.transform

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.sbt

import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** Set the `rootTreeOrProvider` property of class symbols. */
class SetRootTree extends MiniPhase {

  override val phaseName: String = SetRootTree.name
  override def isRunnable(using Context) =
    super.isRunnable && ctx.settings.YretainTrees.value

  // Check no needed. Does not transform trees
  override def isCheckable: Boolean = false
  // override def runsAfter: Set[String] = Set(PostTyper.name)
  // override def runsAfterGroupsOf: Set[String] = Set(Memoize.name)

  // override def run(using Context): Unit = {
  //   val tree = ctx.compilationUnit.tpdTree
  //   traverser.traverse(tree)
  // }

  override def transformTypeDef(td: tpd.TypeDef)(using Context): tpd.Tree = SetRootTree.addRootTree(td)


  // private def traverser = new tpd.TreeTraverser {
  //   override def traverse(tree: tpd.Tree)(using Context): Unit =
  //     tree match {
  //       case pkg: tpd.PackageDef =>
  //         traverseChildren(pkg)
  //       case td: tpd.TypeDef =>
  //         if (td.symbol.isClass) {
  //           val sym = td.symbol.asClass
  //           tpd.sliceTopLevel(ctx.compilationUnit.tpdTree, sym) match {
  //             case (pkg: tpd.PackageDef) :: Nil =>
  //               sym.rootTreeOrProvider = pkg
  //             case _ =>
  //               sym.rootTreeOrProvider = td
  //           }
  //         }
  //       case _ =>
  //         ()
  //     }
  // }
}

object SetRootTree {
  val name: String = "SetRootTree"

  def addRootsInUnit(unit: CompilationUnit)(using Context): Unit = {
    val traverser = new tpd.TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match {
          case pkg: tpd.PackageDef =>
            traverseChildren(pkg)
          case td: tpd.TypeDef =>
            addRootTree(td)
          case _ =>
            ()
        }
    }
    traverser.traverse(unit.tpdTree)
  }

  private def addRootTree(tdef: tpd.TypeDef)(using Context): tdef.type = {
    if (tdef.symbol.isClass) {
      val sym = tdef.symbol.asClass
      tpd.sliceTopLevel(ctx.compilationUnit.tpdTree, sym) match {
        case (pkg: tpd.PackageDef) :: Nil =>
          sym.rootTreeOrProvider = pkg
        case _ =>
          sym.rootTreeOrProvider = tdef
      }
    }
    tdef
  }

}
