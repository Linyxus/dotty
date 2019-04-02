package scala.runtime.quoted

import scala.annotation.internal.sharable

import scala.quoted._
import scala.quoted.matching.Binding
import scala.tasty._

object Matcher {

  private final val debug = false

  /** Pattern representing a quoted.Type splice */
  type Hole[T <: AnyKind] = T

  /** Pattern representing a quoted.matching.Binding
   *  The pattern is placed around the type tree of the `val`, `lazy val`, `var` or `def`
   */
  type bindHole[T] = T

  /** Pattern representing a quoted.Expr splice */
  def hole[T]: T = ???

  /** Pattern representing a quoted.Expr splice in a pattern position */
  object patternHole {
    def unapply[T](arg: Any): Boolean = ???
  }

  /** Pattern representing a quoted.matching.Binding in a pattern position */
  object patternBindHole {
    def unapply[T](arg: Any): Some[T] = ???
  }

  /** Pattern representing a quoted.matching.Binding on the LHS of an assignment */
  @sharable var varHole: Any = null

  /**
   *
   * @param scrutineeExpr
   * @param patternExpr
   * @param reflection
   * @return None if it did not match, Some(tup) if it matched where tup contains Expr[_], Type[_] or Binding[_]
   */
  def unapply[Tup <: Tuple](scrutineeExpr: Expr[_])(implicit patternExpr: Expr[_], reflection: Reflection): Option[Tup] = {
    import reflection._

    def treeMatches(scrutinee: Tree, pattern: Tree)(implicit env: Set[(Symbol, Symbol)]): Option[Tuple] = {

      /** Check that both are `val` or both are `lazy val` or both are `var` **/
      def checkValFlags(): Boolean = {
        import Flags._
        val sFlags = scrutinee.symbol.flags
        val pFlags = pattern.symbol.flags
        sFlags.is(Lazy) == pFlags.is(Lazy) && sFlags.is(Mutable) == pFlags.is(Mutable)
      }

      def isMatcherHole(sym: Symbol): Boolean =
        sym.owner.fullName == "scala.runtime.quoted.Matcher$" // TODO check symbol equality instead of its name

      def treesMatch(scrutinees: List[Tree], patterns: List[Tree]): Option[Tuple] =
        if (scrutinees.size != patterns.size) None
        else foldMatchings(scrutinees.zip(patterns).map(treeMatches): _*)

      def bindingAndTptMatch(sym: Symbol, tpt1: TypeTree, tpt2: TypeTree): (Option[Tuple], Option[Tuple]) = tpt2 match {
        case bindHole @ TypeTree.Applied(TypeIdent("bindHole"), IsTypeTree(tpt2) :: Nil)
            if isMatcherHole(bindHole.symbol) && tpt1.tpe <:< tpt2.tpe => // Is the subtype check required?
          val binding = new Binding(sym.name, sym)
          (Some(Tuple1(binding)), treeMatches(tpt1, tpt2))
        case returnTpt2 =>
          (Some(()), treeMatches(tpt1, tpt2))
      }

      (scrutinee, pattern) match {
        // Normalize blocks without statements
        case (Block(Nil, expr), _) => treeMatches(expr, pattern)
        case (_, Block(Nil, pat)) => treeMatches(scrutinee, pat)

        case (IsTerm(scrutinee), TypeApply(Ident("hole"), tpt :: Nil))
            if isMatcherHole(pattern.symbol) && scrutinee.tpe <:< tpt.tpe =>
          Some(Tuple1(scrutinee.seal))

        case (IsTypeTree(scrutinee), IsTypeTree(pattern @ TypeTree.Applied(TypeIdent("Hole"), IsTypeTree(tpt) :: Nil)))
            if isMatcherHole(pattern.symbol) && scrutinee.tpe <:< tpt.tpe => // Is the subtype check required?
          Some(Tuple1(scrutinee.tpe.seal))

        case (Inlined(_, Nil, scr), _) =>
          treeMatches(scr, pattern)
        case (_, Inlined(_, Nil, pat)) =>
          treeMatches(scrutinee, pat)

        case (Literal(constant1), Literal(constant2)) if constant1 == constant2 =>
          Some(())

        case (Ident(_), Ident(_)) if scrutinee.symbol == pattern.symbol || env((scrutinee.symbol, pattern.symbol)) =>
          Some(())

        case (Typed(expr1, tpt1), Typed(expr2, tpt2)) =>
          foldMatchings(treeMatches(expr1, expr2), treeMatches(tpt1, tpt2))

        case (Select(qual1, _), Select(qual2, _)) if scrutinee.symbol == pattern.symbol =>
          treeMatches(qual1, qual2)

        case (Ident(_), Select(_, _)) if scrutinee.symbol == pattern.symbol =>
          Some(())

        case (Select(_, _), Ident(_)) if scrutinee.symbol == pattern.symbol =>
          Some(())

        case (Apply(fn1, args1), Apply(fn2, args2)) if fn1.symbol == fn2.symbol =>
          foldMatchings(treeMatches(fn1, fn2), treesMatch(args1, args2))

        case (TypeApply(fn1, args1), TypeApply(fn2, args2)) if fn1.symbol == fn2.symbol =>
          foldMatchings(treeMatches(fn1, fn2), treesMatch(args1, args2))

        case (Block(stats1, expr1), Block(stats2, expr2)) =>
          foldMatchings(treesMatch(stats1, stats2), treeMatches(expr1, expr2))

        case (If(cond1, thenp1, elsep1), If(cond2, thenp2, elsep2)) =>
          foldMatchings(treeMatches(cond1, cond2), treeMatches(thenp1, thenp2), treeMatches(elsep1, elsep2))

        case (Assign(lhs1, rhs1), Assign(lhs2, rhs2)) =>
          val lhsMatch = lhs2 match {
            case Ident("varHole") if isMatcherHole(lhs2.symbol) =>
              val sym = lhs1.symbol
              Some(Tuple1(new Binding(sym.name, sym)))
            case _ =>
              if (treeMatches(lhs1, lhs2).isDefined) Some(())
              else None
          }
          foldMatchings(lhsMatch, treeMatches(rhs1, rhs2))

        case (While(cond1, body1), While(cond2, body2)) =>
          foldMatchings(treeMatches(cond1, cond2), treeMatches(body1, body2))

        case (NamedArg(name1, expr1), NamedArg(name2, expr2)) if name1 == name2 =>
          treeMatches(expr1, expr2)

        case (New(tpt1), New(tpt2)) =>
          treeMatches(tpt1, tpt2)

        case (This(_), This(_)) if scrutinee.symbol == pattern.symbol =>
          Some(())

        case (Super(qual1, mix1), Super(qual2, mix2)) if mix1 == mix2 =>
          treeMatches(qual1, qual2)

        case (Repeated(elems1, _), Repeated(elems2, _)) if elems1.size == elems2.size =>
          treesMatch(elems1, elems2)

        case (IsTypeTree(scrutinee @ TypeIdent(_)), IsTypeTree(pattern @ TypeIdent(_))) if scrutinee.symbol == pattern.symbol =>
          Some(())

        case (IsInferred(scrutinee), IsInferred(pattern)) if scrutinee.tpe <:< pattern.tpe =>
          Some(())

        case (Applied(tycon1, args1), Applied(tycon2, args2)) =>
          foldMatchings(treeMatches(tycon1, tycon2), treesMatch(args1, args2))

        case (ValDef(_, tpt1, rhs1), ValDef(_, tpt2, rhs2)) if checkValFlags() =>
          val (bindMatch, returnTptMatch) = bindingAndTptMatch(scrutinee.symbol, tpt1, tpt2)
          val rhsEnv = env + (scrutinee.symbol -> pattern.symbol)
          val rhsMatchings = treeOptMatches(rhs1, rhs2)(rhsEnv)
          foldMatchings(bindMatch, returnTptMatch, rhsMatchings)

        case (DefDef(_, typeParams1, paramss1, tpt1, Some(rhs1)), DefDef(_, typeParams2, paramss2, tpt2, Some(rhs2))) =>
          val typeParmasMatch = treesMatch(typeParams1, typeParams2)
          val paramssMatch =
            if (paramss1.size != paramss2.size) None
            else foldMatchings(paramss1.zip(paramss2).map { (params1, params2) => treesMatch(params1, params2) }: _*)
          val (bindMatch, tptMatch) = bindingAndTptMatch(scrutinee.symbol, tpt1, tpt2)
          val rhsEnv =
            env + (scrutinee.symbol -> pattern.symbol) ++
            typeParams1.zip(typeParams2).map((tparam1, tparam2) => tparam1.symbol -> tparam2.symbol) ++
            paramss1.flatten.zip(paramss2.flatten).map((param1, param2) => param1.symbol -> param2.symbol)
          val rhsMatch = treeMatches(rhs1, rhs2)(rhsEnv)

          foldMatchings(bindMatch, typeParmasMatch, paramssMatch, tptMatch, rhsMatch)

        case (Term.Lambda(_, tpt1), Term.Lambda(_, tpt2)) =>
          // TODO match tpt1 with tpt2?
          Some(())

        case (Term.Match(scru1, cases1), Term.Match(scru2, cases2)) =>
          val scrutineeMacth = treeMatches(scru1, scru2)
          val casesMatch =
            if (cases1.size != cases2.size) None
            else foldMatchings(cases1.zip(cases2).map(caseMatches): _*)
          foldMatchings(scrutineeMacth, casesMatch)

        case (Term.Try(body1, cases1, finalizer1), Term.Try(body2, cases2, finalizer2)) =>
          val bodyMacth = treeMatches(body1, body2)
          val casesMatch =
            if (cases1.size != cases2.size) None
              else foldMatchings(cases1.zip(cases2).map(caseMatches): _*)
          val finalizerMatch = treeOptMatches(finalizer1, finalizer2)
          foldMatchings(bodyMacth, casesMatch, finalizerMatch)

        case (_, Block(stats, expr)) if stats.forall { case IsTypeDef(s) if s.name.toString.startsWith("Binding$") => true; case _ => false } =>
          treeMatches(scrutinee, expr)

        case _ =>
          if (debug)
            println(
              s""">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                 |Scrutinee
                 |  ${scrutinee.showCode}
                 |
                 |${scrutinee.show}
                 |
                 |did not match pattern
                 |  ${pattern.showCode}
                 |
                 |${pattern.show}
                 |
                 |
                 |
                 |
                 |""".stripMargin)
          None
      }
    }

    def treeOptMatches(scrutinee: Option[Tree], pattern: Option[Tree])(implicit env: Set[(Symbol, Symbol)]): Option[Tuple] = {
      (scrutinee, pattern) match {
        case (Some(x), Some(y)) => treeMatches(x, y)
        case (None, None) => Some(())
        case _ => None
      }
    }

    def caseMatches(scrutinee: CaseDef, pattern: CaseDef)(implicit env: Set[(Symbol, Symbol)]): Option[Tuple] = {
      val (caseEnv, patternMatch) = patternMatches(scrutinee.pattern, pattern.pattern)
      val guardMatch = treeOptMatches(scrutinee.guard, pattern.guard)(caseEnv)
      val rhsMatch = treeMatches(scrutinee.rhs, pattern.rhs)(caseEnv)
      foldMatchings(patternMatch, guardMatch, rhsMatch)
    }

    def patternMatches(scrutinee: Pattern, pattern: Pattern)(implicit env: Set[(Symbol, Symbol)]): (Set[(Symbol, Symbol)], Option[Tuple]) = (scrutinee, pattern) match {
      case (Pattern.Value(v1), Pattern.Unapply(Term.TypeApply(Term.Select(patternHole @ Term.Ident("patternHole"), "unapply"), List(tpt)), Nil, Nil))
          if patternHole.symbol.owner.fullName == "scala.runtime.quoted.Matcher$" =>
        (env, Some(Tuple1(v1.seal)))
      case (Pattern.Bind(name1, body1), Pattern.Unapply(Term.TypeApply(Term.Select(patternBindHole @ Term.Ident("patternBindHole"), "unapply"), List(tpt)), Nil, List(pattern2 @ Pattern.Bind(name2, body2))))
          if patternBindHole.symbol.owner.fullName == "scala.runtime.quoted.Matcher$" =>
        val binding = new Binding(scrutinee.symbol.name, scrutinee.symbol)
        val env1 = env + (scrutinee.symbol -> pattern2.symbol)
        val (env2, bodyMatch) = patternMatches(body1, body2)(env1)
        (env2, foldMatchings(Some(Tuple1(binding)), bodyMatch))
      case (Pattern.Value(v1), Pattern.Value(v2)) =>
        (env, treeMatches(v1, v2))
      case (Pattern.Bind(name1, body1), Pattern.Bind(name2, body2)) =>
        val bindEnv = env + (scrutinee.symbol -> pattern.symbol)
        patternMatches(body1, body2)(bindEnv)
      case (Pattern.Unapply(fun1, implicits1, patterns1), Pattern.Unapply(fun2, implicits2, patterns2)) =>
        val funMatch = treeMatches(fun1, fun2)
        val implicitsMatch =
          if (implicits1.size != implicits2.size) None
          else foldMatchings(implicits1.zip(implicits2).map(treeMatches): _*)
        val (patEnv, patternsMatch) = foldPatterns(patterns1, patterns2)
        (patEnv, foldMatchings(funMatch, implicitsMatch, patternsMatch))
      case (Pattern.Alternatives(patterns1), Pattern.Alternatives(patterns2)) =>
        foldPatterns(patterns1, patterns2)
      case (Pattern.TypeTest(tpt1), Pattern.TypeTest(tpt2)) =>
        (env, treeMatches(tpt1, tpt2))
      case _ =>
        if (debug)
          println(
            s""">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
               |Scrutinee
               |  ${scrutinee.showCode}
               |
               |${scrutinee.show}
               |
               |did not match pattern
               |  ${pattern.showCode}
               |
               |${pattern.show}
               |
               |
               |
               |
               |""".stripMargin)
        (env, None)
    }

    def foldPatterns(patterns1: List[Pattern], patterns2: List[Pattern])(implicit env: Set[(Symbol, Symbol)]): (Set[(Symbol, Symbol)], Option[Tuple]) = {
      if (patterns1.size != patterns2.size) (env, None)
      else patterns1.zip(patterns2).foldLeft((env, Option[Tuple](()))) { (acc, x) =>
        val (env, res) = patternMatches(x._1, x._2)(acc._1)
        (env, foldMatchings(acc._2, res))
      }
    }

    treeMatches(scrutineeExpr.unseal, patternExpr.unseal)(Set.empty).asInstanceOf[Option[Tup]]
  }

  private def foldMatchings(matchings: Option[Tuple]*): Option[Tuple] = {
    matchings.foldLeft[Option[Tuple]](Some(())) {
      case (Some(acc), Some(holes)) => Some(acc ++ holes)
      case (_, _) => None
    }
  }

}
