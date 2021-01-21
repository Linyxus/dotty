package dotty.tools.tasty


import org.junit.Assert._
import org.junit.Test

import TastyFormat._

import ScalaVersionTest._

class ScalaVersionTest {

  @Test def showVersion: Unit = {
    assertEquals("3.1.0", showScalaVersionString(scala31Release))
    assertEquals("3.0.0-M4-SNAPSHOT", showScalaVersionString(scala3m4Snapshot))
    assertEquals("32767.32767.32767-RC4095-NIGHTLY", showScalaVersionString(maximumScalaVersion))
  }

  @Test def packLong: Unit = {

    def direct(version: ScalaVersionString) = {
      val encoded = encodeVersionString(version)
      val decodedDirect = decodeVersionString(encoded)
      assertEquals(version,decodedDirect)
    }

    direct(scala31Release)
    direct(scala3m4Snapshot)
    direct(maximumScalaVersion)
  }

  @Test def packBuffer: Unit = {

    def throughBuf(version: ScalaVersionString) = {
      val buf = new TastyBuffer(128)
      buf.writeUncompressedLong(encodeVersionString(version))
      val reader = new TastyReader(buf.bytes.clone)
      val encoded = reader.readUncompressedLong()
      val decodedBuffer = decodeVersionString(encoded)
      assertEquals(version,decodedBuffer)
    }

    throughBuf(scala31Release)
    throughBuf(scala3m4Snapshot)
    throughBuf(maximumScalaVersion)
  }

}

object ScalaVersionTest {

  val scala31Release = ScalaVersionString(
    maj   = 3,
    min   = 1,
    patch = 0,
    release = ScalaRelease(
      experimental = ScalaExperimentalKind.Empty,
      kind         = ScalaReleaseKind.Final,
    )
  )

  val scala3m4Snapshot = ScalaVersionString(
    maj   = 3,
    min   = 0,
    patch = 0,
    release = ScalaRelease(
      experimental = ScalaExperimentalKind.Snapshot,
      kind         = ScalaReleaseKind.Milestone(4),
    )
  )

  val maximumScalaVersion = ScalaVersionString(
    maj   = 32767,
    min   = 32767,
    patch = 32767,
    release = ScalaRelease(
      experimental = ScalaExperimentalKind.Nightly,
      kind         = ScalaReleaseKind.ReleaseCandidate(4095),
    )
  )

  sealed trait ScalaExperimentalKind

  object ScalaExperimentalKind {
    case object Snapshot extends ScalaExperimentalKind
    case object Nightly  extends ScalaExperimentalKind
    case object Empty    extends ScalaExperimentalKind
  }

  sealed trait ScalaReleaseKind

  object ScalaReleaseKind {
    case object Final                        extends ScalaReleaseKind
    case class  Milestone(patch: Int)        extends ScalaReleaseKind
    case class  ReleaseCandidate(patch: Int) extends ScalaReleaseKind
  }

  case class ScalaRelease(experimental: ScalaExperimentalKind, kind: ScalaReleaseKind)

  case class ScalaVersionString(maj: Int, min: Int, patch: Int, release: ScalaRelease)

  def showScalaReleaseKind(sr: ScalaReleaseKind) = sr match {
    case ScalaReleaseKind.Final => ""
    case ScalaReleaseKind.Milestone(patch) => s"-M$patch"
    case ScalaReleaseKind.ReleaseCandidate(patch) => s"-RC$patch"
  }

  def showScalaExperimentalKind(sr: ScalaExperimentalKind) = sr match {
    case ScalaExperimentalKind.Snapshot => "-SNAPSHOT"
    case ScalaExperimentalKind.Nightly => "-NIGHTLY"
    case ScalaExperimentalKind.Empty => ""
  }

  def showScalaRelease(sr: ScalaRelease) = {
    s"${showScalaReleaseKind(sr.kind)}${showScalaExperimentalKind(sr.experimental)}"
  }

  def showScalaVersionString(sv: ScalaVersionString) = {
    val release = showScalaRelease(sv.release)
    s"${sv.maj}.${sv.min}.${sv.patch}$release"
  }

  def encodeVersionString(v: ScalaVersionString): Long = {
    val relexp = v.release.experimental match {
      case ScalaExperimentalKind.Snapshot => ExperimentalTags.SNAPSHOT
      case ScalaExperimentalKind.Nightly => ExperimentalTags.NIGHTLY
      case ScalaExperimentalKind.Empty => ExperimentalTags.EMPTY
    }
    val (relkind, relpatch) = v.release.kind match {
      case ScalaReleaseKind.Final => (ReleaseTags.FINAL, 0)
      case ScalaReleaseKind.Milestone(patch) => (ReleaseTags.MILESTONE, patch)
      case ScalaReleaseKind.ReleaseCandidate(patch) => (ReleaseTags.RELEASEcandidate, patch)
    }
    encodeScalaVersion(
      v.maj,
      v.min,
      v.patch,
      encodeRelease(
        relexp,
        relkind,
        relpatch))
  }

  def decodeVersionString(v: Long): ScalaVersionString = {
    val maj = DecodeScalaVersion.major(v)
    val min = DecodeScalaVersion.minor(v)
    val pat = DecodeScalaVersion.patch(v)
    val rel = DecodeScalaVersion.release(v)

    val e = DecodeRelease.experimental(rel) match {
      case ExperimentalTags.SNAPSHOT => ScalaExperimentalKind.Snapshot
      case ExperimentalTags.NIGHTLY => ScalaExperimentalKind.Nightly
      case ExperimentalTags.EMPTY => ScalaExperimentalKind.Empty
    }
    val t = DecodeRelease.patch(rel)
    val k = DecodeRelease.kind(rel) match {
      case ReleaseTags.FINAL => ScalaReleaseKind.Final
      case ReleaseTags.MILESTONE => ScalaReleaseKind.Milestone(t)
      case ReleaseTags.RELEASEcandidate => ScalaReleaseKind.ReleaseCandidate(t)
    }
    ScalaVersionString(maj, min, pat, ScalaRelease(e, k))
  }
}
