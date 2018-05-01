package scala.tasty.util

import scala.tasty.Tasty

abstract class TreeAccumulator[X](implicit val tasty: Tasty) {
  import tasty._

  // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
  def foldTree(x: X, tree: TopLevelStatement)(implicit ctx: Context): X
  def foldTypeTree(x: X, tree: MaybeTypeTree)(implicit ctx: Context): X
  def foldCaseDef(x: X, tree: CaseDef)(implicit ctx: Context): X
  def foldPattern(x: X, tree: Pattern)(implicit ctx: Context): X
  def foldParent(x: X, tree: Parent)(implicit ctx: Context): X

  def foldTree(x: X, trees: Traversable[TopLevelStatement])(implicit ctx: Context): X = (x /: trees)(foldTree)
  def foldTypeTree(x: X, trees: Traversable[MaybeTypeTree])(implicit ctx: Context): X = (x /: trees)(foldTypeTree)
  def foldCaseDef(x: X, trees: Traversable[CaseDef])(implicit ctx: Context): X = (x /: trees)(foldCaseDef)
  def foldPattern(x: X, trees: Traversable[Pattern])(implicit ctx: Context): X = (x /: trees)(foldPattern)
  def foldParent(x: X, trees: Traversable[Parent])(implicit ctx: Context): X = (x /: trees)(foldParent)

  def foldOverTree(x: X, tree: TopLevelStatement)(implicit ctx: Context): X = {
    def localCtx(definition: Definition): Context = definition.localContext
    tree match {
      case Ident(name) =>
        x
      case Select(qualifier, name) =>
        foldTree(x, qualifier)
      case This(qual) =>
        x
      case Super(qual, mix) =>
        foldTree(x, qual)
      case Apply(fun, args) =>
        foldTree(foldTree(x, fun), args)
      case TypeApply(fun, args) =>
        foldTree(foldTree(x, fun), args)
      case Literal(const) =>
        x
      case New(tpt) =>
        foldTypeTree(x, tpt)
      case Typed(expr, tpt) =>
        foldTypeTree(foldTree(x, expr), tpt)
      case NamedArg(name, arg) =>
        foldTree(x, arg)
      case Assign(lhs, rhs) =>
        foldTree(foldTree(x, lhs), rhs)
      case Block(stats, expr) =>
        foldTree(foldTree(x, stats), expr)
      case If(cond, thenp, elsep) =>
        foldTree(foldTree(foldTree(x, cond), thenp), elsep)
      case Lambda(meth, tpt) =>
        val a = foldTree(x, meth)
        tpt.fold(a)(b => foldTypeTree(a, b))
      case Match(selector, cases) =>
        foldCaseDef(foldTree(x, selector), cases)
      case Return(expr) =>
        foldTree(x, expr)
      case Try(block, handler, finalizer) =>
        foldTree(foldCaseDef(foldTree(x, block), handler), finalizer)
      case Repeated(elems) =>
        foldTree(x, elems)
      case Inlined(call, bindings, expansion) =>
        foldTree(foldTree(x, bindings), expansion)

      case Definition(vdef @ ValDef(_, tpt, rhs)) =>
        implicit val ctx = localCtx(vdef)
        foldTree(foldTypeTree(x, tpt), rhs)
      case Definition(ddef @ DefDef(_, tparams, vparamss, tpt, rhs)) =>
        implicit val ctx = localCtx(ddef)
        foldTree(foldTypeTree((foldTree(x, tparams) /: vparamss)(foldTree), tpt), rhs)
      case Definition(tdef @ TypeDef(name, rhs)) =>
        implicit val ctx = localCtx(tdef)
        foldTypeTree(x, rhs)
      case Definition(cdef @ ClassDef(_, constr, parents, self, body)) =>
        implicit val ctx = localCtx(cdef)
        foldTree(foldTree(foldParent(foldTree(x, constr), parents), self), body)
      case Import(expr, selectors) =>
        foldTree(x, expr)
      case clause @ PackageClause(pid, stats) =>
        foldTree(foldTree(x, pid), stats)(localCtx(??? /*clause.definition*/))
      case _ =>
        x
    }
  }

  def foldOverTypeTree(x: X, tree: MaybeTypeTree)(implicit ctx: Context): X = tree match {
    case TypeIdent(name) => x
    case TypeSelect(qualifier, name) => foldTree(x, qualifier)
    case Singleton(ref) => foldTree(x, ref)
    case And(left, right) => foldTypeTree(foldTypeTree(x, left), right)
    case Or(left, right) => foldTypeTree(foldTypeTree(x, left), right)
    case Refined(tpt, refinements) => foldTree(foldTypeTree(x, tpt), refinements)
    case Applied(tpt, args) => foldTypeTree(foldTypeTree(x, tpt), args)
    case ByName(result) => foldTypeTree(x, result)
    case TypeBoundsTree(lo, hi) => foldTypeTree(foldTypeTree(x, lo), hi)
    case Annotated(arg, annot) => foldTree(foldTypeTree(x, arg), annot)
  }

  def foldOverCaseDef(x: X, tree: CaseDef)(implicit ctx: Context): X = tree match {
    case CaseDef(pat, guard, body) => foldTree(foldTree(foldPattern(x, pat), guard), body)
  }

  def foldOverPattern(x: X, tree: Pattern)(implicit ctx: Context): X = tree match {
    case Value(v) => foldTree(x, v)
    case Bind(_, body) => foldPattern(x, body)
    case Unapply(fun, implicits, patterns) => foldPattern(foldTree(foldTree(x, fun), implicits), patterns)
    case Alternative(patterns) => foldPattern(x, patterns)
    case TypeTest(tpt) => foldTypeTree(x, tpt)
  }

  def foldOverParent(x: X, tree: Parent)(implicit ctx: Context): X = {
    ???
  }

}
