package japgolly.microlibs.utils

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.{Codec, Source}

object FileUtils {

  /** Add to SBT:
   *
   * {{{
   *   javaOptions += ("-DbaseDirectory=" + baseDirectory.value.getAbsolutePath)
   * }}}
   */
  lazy val baseDirectory: File = {
    val p = "baseDirectory"
    val d = Option(System.getProperty(p)).getOrElse(sys.error(s"Property [$p] isn't specified."))
    val f = new File(d)
    require(f.exists() && f.isDirectory(), s"Directory not found: ${f.getAbsolutePath}")
    f
  }

  def baseDirectoryFile(suffix: String): File = {
    val f = baseDirectory
    new File(s"${f.getAbsolutePath}/$suffix")
  }

  def testResourceFile(path: String): File =
    baseDirectoryFile(s"src/test/resources/$path")

  def write(filename: String, content: String): Unit = {
    Files.write(Paths.get(filename), content.getBytes(StandardCharsets.UTF_8))
    ()
  }

  def read(filename: String): String = {
    val src = Source.fromFile(filename)(Codec.UTF8)
    try src.mkString finally src.close()
  }

  def readResource(filename: String): String = {
    val res = if (filename.startsWith("/")) filename else "/" + filename
    val src = Source.fromResource(res)(Codec.UTF8)
    try src.mkString finally src.close()
  }

}
