package dotty.tools

import vulpix.TestConfiguration

import org.junit.Test

import dotc.ast.Trees._
import dotc.core.Decorators._
import dotc.core.Contexts._
import dotc.core.Phases._
import dotc.core.Types._
import dotc.core.Symbols._
import dotc.core.Signature
import dotc.core.TypeErasure.sigName

import java.io.File
import java.nio.file._

class SignatureTest:
  @Test def signatureCaching: Unit =
    inCompilerContext(TestConfiguration.basicClasspath, separateRun = true, "case class Foo(value: Unit)") {
      val (ref, refSig) = atPhase(erasurePhase.next) {
        val cls = requiredClass("Foo")
        val ref = cls.requiredMethod("value").termRef
        (ref, ref.signature)
      }
      atPhase(typerPhase) {
        // NamedType#signature is always computed before erasure, which ensures
        // that it stays stable and therefore can be cached as long as
        // signatures are guaranteed to be stable before erasure, see the
        // comment above `Compiler#phases`.
        assert(refSig == ref.signature,
          s"""The signature of a type should never change but the signature of $ref was:
             |${ref.signature} at typer, whereas it was:
             |${refSig} after erasure""".stripMargin)
        assert(ref.signature == ref.denot.signature,
          s"""Before erasure, the signature of a TypeRef should be the signature of its denotation,
             |but the cached signature of $ref was:
             |${ref.signature}, whereas its denotation signature at typer was:
             |${ref.denot.signature}""".stripMargin)
      }
    }

  @Test def prefixDependentJavaness: Unit =
    inCompilerContext(TestConfiguration.basicClasspath, separateRun = false,
    """trait MyComparable[T]  { def compareTo(x: Array[T]): Int }
      |trait MyComparable2[T] { def compareTo(x: Array[T]): Int }
      |class Foo[T] {
      |  val jj: Comparable[Array[T]] & Serializable = ???
      |  val js: Comparable[Array[T]] & MyComparable[T] = ???
      |  val sj: MyComparable[T] & Comparable[Array[T]] = ???
      |  val ss: MyComparable[T] & MyComparable2[T] = ???
      |}""".stripMargin) {

      atPhase(picklerPhase) {
        val cls = requiredClass("Foo")

        def checkSignature(prefix: String, expectedSig: Signature) =
          val meth = cls.requiredMethodRef(prefix)
          val denot = meth.select("compareTo".toTermName)
          val actualSig = denot.signature
          assert(actualSig == expectedSig,
            s"""Incorrect signature found for $denot with prefix $meth
               |Expected: $expectedSig
               |Actual  : $actualSig""")

        val objSigName = defn.ObjectClass.fullName.asTypeName
        val resSigName = defn.IntClass.fullName.asTypeName
        val javaSig = Signature(List(objSigName ++ "[]"), resSigName)
        val scalaSig = Signature(List(objSigName), resSigName)

        // If all parts of the prefix are Java-defined, we use the Java
        // signature, otherwise we use the Scala signature.
        checkSignature("jj", javaSig)
        checkSignature("js", scalaSig)
        checkSignature("sj", scalaSig)
        checkSignature("ss", scalaSig)
      }
    }
