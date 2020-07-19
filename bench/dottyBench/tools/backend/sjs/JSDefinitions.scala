package dottyBench.tools.backend.sjs

import dottyBench.tools.dotc.core._

import scala.annotation.threadUnsafe
import Types._
import Contexts._
import Symbols._
import Names._
import StdNames._
import Decorators._

import dottyBench.tools.dotc.config.SJSPlatform

object JSDefinitions {
  /** The Scala.js-specific definitions for the current context. */
  def jsdefn(using Ctx, CState): JSDefinitions =
    ctx.platform.asInstanceOf[SJSPlatform].jsDefinitions
}

final class JSDefinitions()(using Ctx, CState) {

  @threadUnsafe lazy val InlineAnnotType: TypeRef = requiredClassRef("scala.inline")
  def InlineAnnot(using Ctx, CState) = InlineAnnotType.symbol.asClass
  @threadUnsafe lazy val NoinlineAnnotType: TypeRef = requiredClassRef("scala.noinline")
  def NoinlineAnnot(using Ctx, CState) = NoinlineAnnotType.symbol.asClass

  @threadUnsafe lazy val JavaLangVoidType: TypeRef = requiredClassRef("java.lang.Void")
  def JavaLangVoidClass(using Ctx, CState) = JavaLangVoidType.symbol.asClass

  @threadUnsafe lazy val ScalaJSJSPackageVal = requiredPackage("scala.scalajs.js")
  @threadUnsafe lazy val ScalaJSJSPackageClass = ScalaJSJSPackageVal.moduleClass.asClass
    @threadUnsafe lazy val JSPackage_typeOfR = ScalaJSJSPackageClass.requiredMethodRef("typeOf")
    def JSPackage_typeOf(using Ctx, CState) = JSPackage_typeOfR.symbol
    @threadUnsafe lazy val JSPackage_constructorOfR = ScalaJSJSPackageClass.requiredMethodRef("constructorOf")
    def JSPackage_constructorOf(using Ctx, CState) = JSPackage_constructorOfR.symbol
    @threadUnsafe lazy val JSPackage_nativeR = ScalaJSJSPackageClass.requiredMethodRef("native")
    def JSPackage_native(using Ctx, CState) = JSPackage_nativeR.symbol

  @threadUnsafe lazy val JSNativeAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.native")
  def JSNativeAnnot(using Ctx, CState) = JSNativeAnnotType.symbol.asClass

  @threadUnsafe lazy val JSAnyType: TypeRef = requiredClassRef("scala.scalajs.js.Any")
  def JSAnyClass(using Ctx, CState) = JSAnyType.symbol.asClass
  @threadUnsafe lazy val JSObjectType: TypeRef = requiredClassRef("scala.scalajs.js.Object")
  def JSObjectClass(using Ctx, CState) = JSObjectType.symbol.asClass
  @threadUnsafe lazy val JSBaseThisFunctionType: TypeRef = requiredClassRef("scala.scalajs.js.ThisFunction")
  def JSBaseThisFunctionClass(using Ctx, CState) = JSBaseThisFunctionType.symbol.asClass

  @threadUnsafe lazy val JSArrayType: TypeRef = requiredClassRef("scala.scalajs.js.Array")
  def JSArrayClass(using Ctx, CState) = JSArrayType.symbol.asClass

  @threadUnsafe lazy val JSFunctionType = (0 to 22).map(n => requiredClassRef("scala.scalajs.js.Function" + n)).toArray
  def JSFunctionClass(n: Int)(using Ctx, CState) = JSFunctionType(n).symbol.asClass
  @threadUnsafe lazy val JSThisFunctionType = (0 to 21).map(n => requiredClassRef("scala.scalajs.js.ThisFunction" + n)).toArray
  def JSThisFunctionClass(n: Int)(using Ctx, CState) = JSThisFunctionType(n).symbol.asClass

  @threadUnsafe lazy val RuntimeExceptionType: TypeRef = requiredClassRef("java.lang.RuntimeException")
  def RuntimeExceptionClass(using Ctx, CState) = RuntimeExceptionType.symbol.asClass
  @threadUnsafe lazy val JavaScriptExceptionType: TypeRef = requiredClassRef("scala.scalajs.js.JavaScriptException")
  def JavaScriptExceptionClass(using Ctx, CState) = JavaScriptExceptionType.symbol.asClass

  @threadUnsafe lazy val JSGlobalScopeAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSGlobalScope")
  def JSGlobalScopeAnnot(using Ctx, CState) = JSGlobalScopeAnnotType.symbol.asClass
  @threadUnsafe lazy val JSNameAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSName")
  def JSNameAnnot(using Ctx, CState) = JSNameAnnotType.symbol.asClass
  @threadUnsafe lazy val JSFullNameAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSFullName")
  def JSFullNameAnnot(using Ctx, CState) = JSFullNameAnnotType.symbol.asClass
  @threadUnsafe lazy val JSBracketAccessAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSBracketAccess")
  def JSBracketAccessAnnot(using Ctx, CState) = JSBracketAccessAnnotType.symbol.asClass
  @threadUnsafe lazy val JSBracketCallAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSBracketCall")
  def JSBracketCallAnnot(using Ctx, CState) = JSBracketCallAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSExport")
  def JSExportAnnot(using Ctx, CState) = JSExportAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportDescendentObjectsAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSExportDescendentObjects")
  def JSExportDescendentObjectsAnnot(using Ctx, CState) = JSExportDescendentObjectsAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportDescendentClassesAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSExportDescendentClasses")
  def JSExportDescendentClassesAnnot(using Ctx, CState) = JSExportDescendentClassesAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportAllAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSExportAll")
  def JSExportAllAnnot(using Ctx, CState) = JSExportAllAnnotType.symbol.asClass
  @threadUnsafe lazy val JSExportNamedAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.JSExportNamed")
  def JSExportNamedAnnot(using Ctx, CState) = JSExportNamedAnnotType.symbol.asClass
  @threadUnsafe lazy val RawJSTypeAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.RawJSType")
  def RawJSTypeAnnot(using Ctx, CState) = RawJSTypeAnnotType.symbol.asClass
  @threadUnsafe lazy val ExposedJSMemberAnnotType: TypeRef = requiredClassRef("scala.scalajs.js.annotation.ExposedJSMember")
  def ExposedJSMemberAnnot(using Ctx, CState) = ExposedJSMemberAnnotType.symbol.asClass

  @threadUnsafe lazy val JSAnyModuleRef = requiredModuleRef("scala.scalajs.js.Any")
  def JSAnyModule(using Ctx, CState) = JSAnyModuleRef.symbol
    @threadUnsafe lazy val JSAny_fromFunctionR = (0 to 22).map(n => JSAnyModule.requiredMethodRef("fromFunction" + n)).toArray
    def JSAny_fromFunction(n: Int)(using Ctx, CState) = JSAny_fromFunctionR(n).symbol

  @threadUnsafe lazy val JSDynamicModuleRef = requiredModuleRef("scala.scalajs.js.Dynamic")
  def JSDynamicModule(using Ctx, CState) = JSDynamicModuleRef.symbol
    @threadUnsafe lazy val JSDynamic_globalR = JSDynamicModule.requiredMethodRef("global")
    def JSDynamic_global(using Ctx, CState) = JSDynamic_globalR.symbol
    @threadUnsafe lazy val JSDynamic_newInstanceR = JSDynamicModule.requiredMethodRef("newInstance")
    def JSDynamic_newInstance(using Ctx, CState) = JSDynamic_newInstanceR.symbol

  @threadUnsafe lazy val JSDynamicLiteralModuleRef = JSDynamicModule.moduleClass.requiredValueRef("literal")
  def JSDynamicLiteralModule(using Ctx, CState) = JSDynamicLiteralModuleRef.symbol
    @threadUnsafe lazy val JSDynamicLiteral_applyDynamicNamedR = JSDynamicLiteralModule.requiredMethodRef("applyDynamicNamed")
    def JSDynamicLiteral_applyDynamicNamed(using Ctx, CState) = JSDynamicLiteral_applyDynamicNamedR.symbol
    @threadUnsafe lazy val JSDynamicLiteral_applyDynamicR = JSDynamicLiteralModule.requiredMethodRef("applyDynamic")
    def JSDynamicLiteral_applyDynamic(using Ctx, CState) = JSDynamicLiteral_applyDynamicR.symbol

  @threadUnsafe lazy val JSObjectModuleRef = requiredModuleRef("scala.scalajs.js.Object")
  def JSObjectModule(using Ctx, CState) = JSObjectModuleRef.symbol

  @threadUnsafe lazy val JSArrayModuleRef = requiredModuleRef("scala.scalajs.js.Array")
  def JSArrayModule(using Ctx, CState) = JSArrayModuleRef.symbol
    @threadUnsafe lazy val JSArray_applyR = JSArrayModule.requiredMethodRef(nme.apply)
    def JSArray_apply(using Ctx, CState) = JSArray_applyR.symbol

  @threadUnsafe lazy val JSThisFunctionModuleRef = requiredModuleRef("scala.scalajs.js.ThisFunction")
  def JSThisFunctionModule(using Ctx, CState) = JSThisFunctionModuleRef.symbol
    @threadUnsafe lazy val JSThisFunction_fromFunctionR = (1 to 22).map(n => JSThisFunctionModule.requiredMethodRef("fromFunction" + n)).toArray
    def JSThisFunction_fromFunction(n: Int)(using Ctx, CState) = JSThisFunction_fromFunctionR(n - 1).symbol

  @threadUnsafe lazy val JSConstructorTagModuleRef = requiredModuleRef("scala.scalajs.js.ConstructorTag")
  def JSConstructorTagModule(using Ctx, CState) = JSConstructorTagModuleRef.symbol
    @threadUnsafe lazy val JSConstructorTag_materializeR = JSConstructorTagModule.requiredMethodRef("materialize")
    def JSConstructorTag_materialize(using Ctx, CState) = JSConstructorTag_materializeR.symbol

  @threadUnsafe lazy val RuntimePackageVal = requiredPackage("scala.scalajs.runtime")
  @threadUnsafe lazy val RuntimePackageClass = RuntimePackageVal.moduleClass.asClass
    @threadUnsafe lazy val RuntimePackage_wrapJavaScriptExceptionR = RuntimePackageClass.requiredMethodRef("wrapJavaScriptException")
    def Runtime_wrapJavaScriptException(using Ctx, CState) = RuntimePackage_wrapJavaScriptExceptionR.symbol
    @threadUnsafe lazy val Runtime_unwrapJavaScriptExceptionR = RuntimePackageClass.requiredMethodRef("unwrapJavaScriptException")
    def Runtime_unwrapJavaScriptException(using Ctx, CState) = Runtime_unwrapJavaScriptExceptionR.symbol
    @threadUnsafe lazy val Runtime_toScalaVarArgsR = RuntimePackageClass.requiredMethodRef("toScalaVarArgs")
    def Runtime_toScalaVarArgs(using Ctx, CState) = Runtime_toScalaVarArgsR.symbol
    @threadUnsafe lazy val Runtime_toJSVarArgsR = RuntimePackageClass.requiredMethodRef("toJSVarArgs")
    def Runtime_toJSVarArgs(using Ctx, CState) = Runtime_toJSVarArgsR.symbol
    @threadUnsafe lazy val Runtime_constructorOfR = RuntimePackageClass.requiredMethodRef("constructorOf")
    def Runtime_constructorOf(using Ctx, CState) = Runtime_constructorOfR.symbol
    @threadUnsafe lazy val Runtime_newConstructorTagR = RuntimePackageClass.requiredMethodRef("newConstructorTag")
    def Runtime_newConstructorTag(using Ctx, CState) = Runtime_newConstructorTagR.symbol
    @threadUnsafe lazy val Runtime_linkingInfoR = RuntimePackageClass.requiredMethodRef("linkingInfo")
    def Runtime_linkingInfo(using Ctx, CState) = Runtime_linkingInfoR.symbol

  @threadUnsafe lazy val SpecialPackageVal = requiredPackage("scala.scalajs.js.special")
  @threadUnsafe lazy val SpecialPackageClass = SpecialPackageVal.moduleClass.asClass
    @threadUnsafe lazy val Special_debuggerR = SpecialPackageClass.requiredMethodRef("debugger")
    def Special_debugger(using Ctx, CState) = Special_debuggerR.symbol
    @threadUnsafe lazy val Special_deleteR = SpecialPackageClass.requiredMethodRef("delete")
    def Special_delete(using Ctx, CState) = Special_deleteR.symbol
    @threadUnsafe lazy val Special_forinR = SpecialPackageClass.requiredMethodRef("forin")
    def Special_forin(using Ctx, CState) = Special_forinR.symbol
    @threadUnsafe lazy val Special_inR = SpecialPackageClass.requiredMethodRef("in")
    def Special_in(using Ctx, CState) = Special_inR.symbol
    @threadUnsafe lazy val Special_instanceofR = SpecialPackageClass.requiredMethodRef("instanceof")
    def Special_instanceof(using Ctx, CState) = Special_instanceofR.symbol

  @threadUnsafe lazy val WrappedArrayType: TypeRef = requiredClassRef("scala.scalajs.js.WrappedArray")
  def WrappedArrayClass(using Ctx, CState) = WrappedArrayType.symbol.asClass

  @threadUnsafe lazy val ScalaRunTime_isArrayR = defn.ScalaRuntimeModule.requiredMethodRef("isArray", List(???, ???))
  def ScalaRunTime_isArray(using Ctx, CState): Symbol = ScalaRunTime_isArrayR.symbol

  @threadUnsafe lazy val BoxesRunTime_boxToCharacterR = defn.BoxesRunTimeModule.requiredMethodRef("boxToCharacter")
  def BoxesRunTime_boxToCharacter(using Ctx, CState): Symbol = BoxesRunTime_boxToCharacterR.symbol
  @threadUnsafe lazy val BoxesRunTime_unboxToCharR = defn.BoxesRunTimeModule.requiredMethodRef("unboxToChar")
  def BoxesRunTime_unboxToChar(using Ctx, CState): Symbol = BoxesRunTime_unboxToCharR.symbol

  @threadUnsafe lazy val EnableReflectiveInstantiationAnnotType: TypeRef = requiredClassRef("scala.scalajs.reflect.annotation.EnableReflectiveInstantiation")
  def EnableReflectiveInstantiationAnnot(using Ctx, CState) = EnableReflectiveInstantiationAnnotType.symbol.asClass

  @threadUnsafe lazy val ReflectModuleRef = requiredModuleRef("scala.scalajs.reflect.Reflect")
  def ReflectModule(using Ctx, CState) = ReflectModuleRef.symbol
    @threadUnsafe lazy val Reflect_registerLoadableModuleClassR = ReflectModule.requiredMethodRef("registerLoadableModuleClass")
    def Reflect_registerLoadableModuleClass(using Ctx, CState) = Reflect_registerLoadableModuleClassR.symbol
    @threadUnsafe lazy val Reflect_registerInstantiatableClassR = ReflectModule.requiredMethodRef("registerInstantiatableClass")
    def Reflect_registerInstantiatableClass(using Ctx, CState) = Reflect_registerInstantiatableClassR.symbol

  private var allRefClassesCache: Set[Symbol] = _
  def allRefClasses(using Ctx, CState): Set[Symbol] = {
    if (allRefClassesCache == null) {
      val baseNames = List("Object", "Boolean", "Character", "Byte", "Short",
          "Int", "Long", "Float", "Double")
      val fullNames = baseNames.flatMap { base =>
        List(s"scala.runtime.${base}Ref", s"scala.runtime.Volatile${base}Ref")
      }
      allRefClassesCache = fullNames.map(name => requiredClass(name)).toSet
    }
    allRefClassesCache
  }

  /** If `cls` is a class in the scala package, its name, otherwise EmptyTypeName */
  private def scalajsClassName(cls: Symbol)(using Ctx, CState): TypeName =
    if (cls.isClass && cls.owner == ScalaJSJSPackageClass) cls.asClass.name
    else EmptyTypeName

  /** Is the given `cls` a class of the form `scala.scalajs.js.prefixN` where
   *  `N` is a number.
   *
   *  This is similar to `isVarArityClass` in `Definitions.scala`.
   */
  private def isScalaJSVarArityClass(cls: Symbol, prefix: String): Boolean = {
    val name = scalajsClassName(cls)
    name.startsWith(prefix) && name.toString.drop(prefix.length).forall(_.isDigit)
  }

  def isJSFunctionClass(cls: Symbol): Boolean =
    isScalaJSVarArityClass(cls, str.Function)

  def isJSThisFunctionClass(cls: Symbol): Boolean =
    isScalaJSVarArityClass(cls, "ThisFunction")

  /** Definitions related to the treatment of JUnit boostrappers. */
  object junit {
    @threadUnsafe lazy val TestAnnotType: TypeRef = requiredClassRef("org.junit.Test")
    def TestAnnotClass(using Ctx, CState): ClassSymbol = TestAnnotType.symbol.asClass

    @threadUnsafe lazy val BeforeAnnotType: TypeRef = requiredClassRef("org.junit.Before")
    def BeforeAnnotClass(using Ctx, CState): ClassSymbol = BeforeAnnotType.symbol.asClass

    @threadUnsafe lazy val AfterAnnotType: TypeRef = requiredClassRef("org.junit.After")
    def AfterAnnotClass(using Ctx, CState): ClassSymbol = AfterAnnotType.symbol.asClass

    @threadUnsafe lazy val BeforeClassAnnotType: TypeRef = requiredClassRef("org.junit.BeforeClass")
    def BeforeClassAnnotClass(using Ctx, CState): ClassSymbol = BeforeClassAnnotType.symbol.asClass

    @threadUnsafe lazy val AfterClassAnnotType: TypeRef = requiredClassRef("org.junit.AfterClass")
    def AfterClassAnnotClass(using Ctx, CState): ClassSymbol = AfterClassAnnotType.symbol.asClass

    @threadUnsafe lazy val IgnoreAnnotType: TypeRef = requiredClassRef("org.junit.Ignore")
    def IgnoreAnnotClass(using Ctx, CState): ClassSymbol = IgnoreAnnotType.symbol.asClass

    @threadUnsafe lazy val BootstrapperType: TypeRef = requiredClassRef("org.scalajs.junit.Bootstrapper")

    @threadUnsafe lazy val TestMetadataType: TypeRef = requiredClassRef("org.scalajs.junit.TestMetadata")

    @threadUnsafe lazy val NoSuchMethodExceptionType: TypeRef = requiredClassRef("java.lang.NoSuchMethodException")

    @threadUnsafe lazy val FutureType: TypeRef = requiredClassRef("scala.concurrent.Future")
    def FutureClass(using Ctx, CState): ClassSymbol = FutureType.symbol.asClass

    @threadUnsafe private lazy val FutureModule_successfulR = requiredModule("scala.concurrent.Future").requiredMethodRef("successful")
    def FutureModule_successful(using Ctx, CState): Symbol = FutureModule_successfulR.symbol

    @threadUnsafe private lazy val SuccessModule_applyR = requiredModule("scala.util.Success").requiredMethodRef(nme.apply)
    def SuccessModule_apply(using Ctx, CState): Symbol = SuccessModule_applyR.symbol
  }

}