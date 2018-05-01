package scala.tasty

abstract class Tasty {

  // ===== Quotes ===================================================

  trait AbstractQuotedExpr {
    def toTasty(implicit ctx: Context): Term
  }
  implicit def QuotedExprDeco[T](x: quoted.Expr[T]): AbstractQuotedExpr

  trait AbstractQuotedType {
    def toTasty(implicit ctx: Context): TypeTree
  }
  implicit def QuotedTypeDeco[T](x: quoted.Type[T]): AbstractQuotedType

  // ===== Contexts =================================================

  type Context

  trait AbstractContext {
    def owner: Definition
  }
  implicit def ContextDeco(x: Context): AbstractContext

  // ===== Id =======================================================

  type Id

  implicit def IdDeco(x: Id): Positioned

  val Id: IdExtractor
  abstract class IdExtractor {
    def unapply(x: Any /*Id*/): Option[String]
  }

  // ===== Trees ====================================================

  // ----- Top Level Statements -----------------------------------------------

  type TopLevelStatement
  implicit def TopLevelStatementDeco(t: TopLevelStatement): Positioned

  type PackageClause <: TopLevelStatement

  val PackageClause: PackageClauseExtractor
  abstract class PackageClauseExtractor {
    def unapply(x: Any /*PackageClause*/)(implicit ctx: Context): Option[(Term, List[TopLevelStatement])]
  }

  trait AbstractPackageClause {
    def definition: Definition
  }
  implicit def PackageClauseDeco(x: PackageClause): AbstractPackageClause

  // ----- Statements -----------------------------------------------

  type Statement <: TopLevelStatement

  type Import <: Statement

  val Import: ImportExtractor
  abstract class ImportExtractor {
    def unapply(x: Any /*Import*/)(implicit ctx: Context): Option[(Term, List[ImportSelector])]
  }

  type ImportSelector

  val SimpleSelector: SimpleSelectorExtractor
  abstract class SimpleSelectorExtractor {
    def unapply(x: Any /*ImportSelector*/)(implicit ctx: Context): Option[Id]
  }

  val RenameSelector: RenameSelectorExtractor
  abstract class RenameSelectorExtractor {
    def unapply(x: Any /*ImportSelector*/)(implicit ctx: Context): Option[(Id, Id)]
  }

  val OmitSelector: OmitSelectorExtractor
  abstract class OmitSelectorExtractor {
    def unapply(x: Any /*ImportSelector*/)(implicit ctx: Context): Option[Id]
  }

  // ----- Definitions ----------------------------------------------

  type Definition <: Statement

  val Definition: DefinitionExtractor
  abstract class DefinitionExtractor {
    def unapply(x: Any /*Definition*/)(implicit ctx: Context): Option[Definition]
  }

  trait AbstractDefinition {
    def mods(implicit ctx: Context): List[Modifier]
    def owner(implicit ctx: Context): Definition
    def localContext(implicit ctx: Context): Context
  }
  implicit def DefinitionDeco(x: Definition): AbstractDefinition

  type Parent // Term | TypeTree

  type ClassDef <: Definition
  val ClassDef: ClassDefExtractor
  abstract class ClassDefExtractor {
    def unapply(x: Any /*ClassDef*/)(implicit ctx: Context): Option[(String, DefDef, List[Parent] /* List[Term | TypeTree] */,  Option[ValDef], List[Statement])]
  }

  type DefDef <: Definition
  val DefDef: DefDefExtractor
  abstract class DefDefExtractor {
    def unapply(x: Any /*DefDef*/)(implicit ctx: Context): Option[(String, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term])]
  }

  type ValDef <: Definition
  val ValDef: ValDefExtractor
  abstract class ValDefExtractor {
    def unapply(x: Any /*ValDef*/)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])]
  }

  type TypeDef <: Definition
  val TypeDef: TypeDefExtractor
  abstract class TypeDefExtractor {
    def unapply(x: Any /*TypeDef*/)(implicit ctx: Context): Option[(String, MaybeTypeTree /* TypeTree | TypeBoundsTree */)]
  }

//  type PackageDef <: Definition
//  val PackageDef: PackageDefExtractor
//  abstract class PackageDefExtractor {
//    def unapply(x: PackageDef)(implicit ctx: Context): Option[(Name, List[Statement])]
//  }

  // ----- Terms ----------------------------------------------------

  type Term <: Statement with Parent
  implicit def TermDeco(t: Term): Typed

  val Ident: IdentExtractor
  abstract class IdentExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[String]
  }

  val Select: SelectExtractor
  abstract class SelectExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, String)]
  }

  val Literal: LiteralExtractor
  abstract class LiteralExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[Constant]
  }

  val This: ThisExtractor
  abstract class ThisExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[Option[Id]]
  }

  val New: NewExtractor
  abstract class NewExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[TypeTree]
  }

  val NamedArg: NamedArgExtractor
  abstract class NamedArgExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(String, Term)]
  }

  val Apply: ApplyExtractor
  abstract class ApplyExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, List[Term])]
  }

  val TypeApply: TypeApplyExtractor
  abstract class TypeApplyExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, List[Term])]
  }

  val Super: SuperExtractor
  abstract class SuperExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, Option[Id])]
  }

  val Typed: TypedExtractor
  abstract class TypedExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, TypeTree)]
  }

  val Assign: AssignExtractor
  abstract class AssignExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, Term)]
  }

  val Block: BlockExtractor
  abstract class BlockExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(List[Statement], Term)]
  }

  val Inlined: InlinedExtractor
  abstract class InlinedExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, List[Definition], Term)]
  }

  val Lambda: LambdaExtractor
  abstract class LambdaExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, Option[TypeTree])]
  }

  val If: IfExtractor
  abstract class IfExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, Term, Term)]
  }

  val Match: MatchExtractor
  abstract class MatchExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, List[CaseDef])]
  }

  val Try: TryExtractor
  abstract class TryExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])]
  }

  val Return: ReturnExtractor
  abstract class ReturnExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[Term]
  }

  val Repeated: RepeatedExtractor
  abstract class RepeatedExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[List[Term]]
  }

  val SelectOuter: SelectOuterExtractor
  abstract class SelectOuterExtractor {
    def unapply(x: Any /*Term*/)(implicit ctx: Context): Option[(Term, Int, Type)]
  }

  // ----- CaseDef --------------------------------------------------

  type CaseDef

  val CaseDef: CaseDefExtractor
  abstract class CaseDefExtractor {
    def unapply(x: Any /*CaseDef*/): Option[(Pattern, Option[Term], Term)]
  }

  // ----- Patterns -------------------------------------------------

  type Pattern

  implicit def PatternDeco(x: Pattern): Typed

  val Value: ValueExtractor
  abstract class ValueExtractor {
    def unapply(x: Any /*Pattern*/)(implicit ctx: Context): Option[Term]
  }

  val Bind: BindExtractor
  abstract class BindExtractor {
    def unapply(x: Any /*Pattern*/)(implicit ctx: Context): Option[(String, Pattern)]
  }

  val Unapply: UnapplyExtractor
  abstract class UnapplyExtractor {
    def unapply(x: Any /*Pattern*/)(implicit ctx: Context): Option[(Term, List[Term], List[Pattern])]
  }

  val Alternative: AlternativeExtractor
  abstract class AlternativeExtractor {
    def unapply(x: Any /*Pattern*/)(implicit ctx: Context): Option[List[Pattern]]
  }

  val TypeTest: TypeTestExtractor
  abstract class TypeTestExtractor {
    def unapply(x: Any /*Pattern*/)(implicit ctx: Context): Option[TypeTree]
  }

  // ----- TypeTrees ------------------------------------------------

  type MaybeTypeTree

  trait AbstractMaybeTypeTree {
    def tpe: MaybeType
  }
  implicit def MaybeTypeTreeDeco(x: MaybeTypeTree): AbstractMaybeTypeTree


  // ----- TypeTrees ------------------------------------------------

  type TypeTree <: MaybeTypeTree with Pattern

  implicit def TypeTreeDeco(x: TypeTree): Typed

  val Synthetic: SyntheticExtractor
  abstract class SyntheticExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Boolean
  }

  val TypeIdent: TypeIdentExtractor
  abstract class TypeIdentExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Option[String]
  }

  val TypeSelect: TypeSelectExtractor
  abstract class TypeSelectExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Option[(Term, String)]
  }

  val Singleton: SingletonExtractor
  abstract class SingletonExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Option[Term]
  }

  val Refined: RefinedExtractor
  abstract class RefinedExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Option[(TypeTree, List[Definition])]
  }

  val Applied: AppliedExtractor
  abstract class AppliedExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Option[(TypeTree, List[TypeTree])]
  }

  val Annotated: AnnotatedExtractor
  abstract class AnnotatedExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Option[(TypeTree, Term)]
  }

  val And: AndExtractor
  abstract class AndExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
  }

  val Or: OrExtractor
  abstract class OrExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
  }

  val ByName: ByNameExtractor
  abstract class ByNameExtractor {
    def unapply(x: Any /*TypeTree*/)(implicit ctx: Context): Option[TypeTree]
  }

  // ----- TypeBoundsTrees ------------------------------------------------

  type TypeBoundsTree <: MaybeTypeTree

  trait AbstractTypeBoundsTree {
    def tpe: TypeBounds
  }
  implicit def TypeBoundsTreeDeco(x: TypeBoundsTree): AbstractTypeBoundsTree

  val TypeBoundsTree: TypeBoundsTreeExtractor
  abstract class TypeBoundsTreeExtractor {
    def unapply(x: Any /*TypeBoundsTree*/)(implicit ctx: Context): Option[(TypeTree, TypeTree)]
  }

  // ===== Types ====================================================

  type MaybeType

  trait Typed {
    def tpe: Type
  }

  // ----- Types ----------------------------------------------------

  type Type <: MaybeType

  val ConstantType: ConstantTypeExtractor
  abstract class ConstantTypeExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[Constant]
  }

  val SymRef: SymRefExtractor
  abstract class SymRefExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[(Definition, MaybeType /* Type | NoPrefix */)]
  }

  val NameRef: NameRefExtractor
  abstract class NameRefExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[(String, MaybeType /* Type | NoPrefix */)]
  }

  val SuperType: SuperTypeExtractor
  abstract class SuperTypeExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[(Type, Type)]
  }

  val Refinement: RefinementExtractor
  abstract class RefinementExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[(Type, String, MaybeType /* Type | TypeBounds */)]
  }

  val AppliedType: AppliedTypeExtractor
  abstract class AppliedTypeExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[(Type, List[MaybeType /* Type | TypeBounds */])]
  }

  val AnnotatedType: AnnotatedTypeExtractor
  abstract class AnnotatedTypeExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[(Type, Term)]
  }

  val AndType: AndTypeExtractor
  abstract class AndTypeExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[(Type, Type)]
  }

  val OrType: OrTypeExtractor
  abstract class OrTypeExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[(Type, Type)]
  }

  val ByNameType: ByNameTypeExtractor
  abstract class ByNameTypeExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[Type]
  }

  val ParamRef: ParamRefExtractor
  abstract class ParamRefExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[(LambdaType[_], Int)]
  }

  val ThisType: ThisTypeExtractor
  abstract class ThisTypeExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[Type]
  }

  val RecursiveThis: RecursiveThisExtractor
  abstract class RecursiveThisExtractor {
    def unapply(x: Any /*Type*/)(implicit ctx: Context): Option[RecursiveType]
  }

  type RecursiveType <: Type
  val RecursiveType: RecursiveTypeExtractor
  abstract class RecursiveTypeExtractor {
    def unapply(x: Any /*RecursiveType*/)(implicit ctx: Context): Option[Type]
  }

  // ----- Methodic Types -------------------------------------------

  type LambdaType[ParamInfo <: MaybeType] <: Type

  type MethodType <: LambdaType[Type]

  trait AbstractMethodType {
    def isImplicit: Boolean
    def isErased: Boolean
  }
  implicit def MethodTypeDeco(x: MethodType): AbstractMethodType

  val MethodType: MethodTypeExtractor
  abstract class MethodTypeExtractor {
    def unapply(x: Any /*MethodType*/)(implicit ctx: Context): Option[(List[String], List[Type], Type)]
  }


  type PolyType <: LambdaType[TypeBounds]

  val PolyType: PolyTypeExtractor
  abstract class PolyTypeExtractor {
    def unapply(x: Any /*PolyType*/)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
  }

  type TypeLambda <: LambdaType[TypeBounds]

  val TypeLambda: TypeLambdaExtractor
  abstract class TypeLambdaExtractor {
    def unapply(x: Any /*TypeLambda*/)(implicit ctx: Context): Option[(List[String], List[TypeBounds], Type)]
  }

  // ----- TypeBounds -----------------------------------------------

  type TypeBounds <: MaybeType

  val TypeBounds: TypeBoundsExtractor
  abstract class TypeBoundsExtractor {
    def unapply(x: Any /*TypeBounds*/)(implicit ctx: Context): Option[(Type, Type)]
  }

  // ----- NoPrefix -------------------------------------------------

  type NoPrefix <: MaybeType

  val NoPrefix: NoPrefixExtractor
  abstract class NoPrefixExtractor {
    def unapply(x: Any /*NoPrefix*/)(implicit ctx: Context): Boolean
  }

  // ===== Constants ================================================

  type Constant
  trait AbstractConstant {
    def value: Any
  }
  implicit def ConstantDeco(x: Constant): AbstractConstant

  val UnitConstant: UnitExtractor
  abstract class UnitExtractor {
    def unapply(x: Any /*Constant*/): Boolean
  }

  val NullConstant: NullExtractor
  abstract class NullExtractor {
    def unapply(x: Any /*Constant*/): Boolean
  }

  val BooleanConstant: BooleanExtractor
  abstract class BooleanExtractor {
    def unapply(x: Any /*Constant*/): Option[Boolean]
  }

  val ByteConstant: ByteExtractor
  abstract class ByteExtractor {
    def unapply(x: Any /*Constant*/): Option[Byte]
  }

  val ShortConstant: ShortExtractor
  abstract class ShortExtractor {
    def unapply(x: Any /*Constant*/): Option[Short]
  }

  val CharConstant: CharExtractor
  abstract class CharExtractor {
    def unapply(x: Any /*Constant*/): Option[Char]
  }

  val IntConstant: IntExtractor
  abstract class IntExtractor {
    def unapply(x: Any /*Constant*/): Option[Int]
  }

  val LongConstant: LongExtractor
  abstract class LongExtractor {
    def unapply(x: Any /*Constant*/): Option[Long]
  }

  val FloatConstant: FloatExtractor
  abstract class FloatExtractor {
    def unapply(x: Any /*Constant*/): Option[Float]
  }

  val DoubleConstant: DoubleExtractor
  abstract class DoubleExtractor {
    def unapply(x: Any /*Constant*/): Option[Double]
  }

  val StringConstant: StringExtractor
  abstract class StringExtractor {
    def unapply(x: Any /*Constant*/): Option[String]
  }

  // ===== Modifiers ================================================

  type Modifier

  val Annotation: AnnotationExtractor
  abstract class AnnotationExtractor {
    def unapply(x: Any /*Modifier*/)(implicit ctx: Context): Option[Term]
  }

  val Flags: FlagsExtractor
  abstract class FlagsExtractor {
    def unapply(x: Any /*Modifier*/)(implicit ctx: Context): Option[FlagSet]
  }

  val QualifiedPrivate: QualifiedPrivateExtractor
  abstract class QualifiedPrivateExtractor {
    def unapply(x: Any /*Modifier*/)(implicit ctx: Context): Option[Type]
  }

  val QualifiedProtected: QualifiedProtectedExtractor
  abstract class QualifiedProtectedExtractor {
    def unapply(x: Any /*Modifier*/)(implicit ctx: Context): Option[Type]
  }

  // ===== Positions ================================================

  trait Position {
    def start: Int
    def end: Int

    def sourceFile: java.nio.file.Path

    def startLine: Int
    def startColumn: Int
    def endLine: Int
    def endColumn: Int
  }

  trait Positioned {
    def pos(implicit ctx: Context): Position
  }

}
