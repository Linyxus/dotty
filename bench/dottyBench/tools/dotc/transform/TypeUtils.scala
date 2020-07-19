package dottyBench.tools
package dotc
package transform

import core._
import TypeErasure.ErasedValueType
import Types._
import Contexts._
import Symbols._
import Names.Name

object TypeUtils {
  /** A decorator that provides methods on types
   *  that are needed in the transformer pipeline.
   */
  implicit class TypeUtilsOps(val self: Type) extends AnyVal {

    def isErasedValueType(using Ctx, CState): Boolean =
      self.isInstanceOf[ErasedValueType]

    def isPrimitiveValueType(using Ctx, CState): Boolean =
      self.classSymbol.isPrimitiveValueClass

    def ensureMethodic(using Ctx, CState): Type = self match {
      case self: MethodicType => self
      case _ => if (currentlyAfterErasure) MethodType(Nil, self) else ExprType(self)
    }

    def widenToParents(using Ctx, CState): Type = self.parents match {
      case Nil => self
      case ps => ps.reduceLeft(AndType(_, _))
    }

    /** The arity of this tuple type, which can be made up of EmptyTuple, TupleX and `*:` pairs,
     *  or -1 if this is not a tuple type.
     */
    def tupleArity(using Ctx, CState): Int = self match {
      case AppliedType(tycon, _ :: tl :: Nil) if tycon.isRef(defn.PairClass) =>
        val arity = tl.tupleArity
        if (arity < 0) arity else arity + 1
      case self: TermRef if self.symbol == defn.EmptyTupleModule =>
        0
      case self if defn.isTupleClass(self.classSymbol) =>
        self.dealias.argInfos.length
      case _ =>
        -1
    }

    /** The element types of this tuple type, which can be made up of EmptyTuple, TupleX and `*:` pairs */
    def tupleElementTypes(using Ctx, CState): List[Type] = self match {
      case AppliedType(tycon, hd :: tl :: Nil) if tycon.isRef(defn.PairClass) =>
        hd :: tl.tupleElementTypes
      case self: TermRef if self.symbol == defn.EmptyTupleModule =>
        Nil
      case self if defn.isTupleClass(self.classSymbol) =>
        self.dealias.argInfos
      case _ =>
        throw new AssertionError("not a tuple")
    }

    /** The `*:` equivalent of an instance of a Tuple class */
    def toNestedPairs(using Ctx, CState): Type =
      TypeOps.nestedPairs(tupleElementTypes)

    def refinedWith(name: Name, info: Type)(using Ctx, CState) = RefinedType(self, name, info)

    /** The TermRef referring to the companion of the underlying class reference
     *  of this type, while keeping the same prefix.
     */
    def companionRef(using Ctx, CState): TermRef = self match {
      case self @ TypeRef(prefix, _) if self.symbol.isClass =>
        prefix.select(self.symbol.companionModule).asInstanceOf[TermRef]
      case self: TypeProxy =>
        self.underlying.companionRef
    }
  }
}