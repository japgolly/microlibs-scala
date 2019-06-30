package japgolly.microlibs.testutil

import scala.io.AnsiColor._
import sourcecode.Line

object TestUtilInternals {

  // scala.Console.BOLD exists but not BRIGHT
  // The difference affects OS X
  final val BRIGHT_BLACK   = "\u001b[90m"
  final val BRIGHT_RED     = "\u001b[91m"
  final val BRIGHT_GREEN   = "\u001b[92m"
  final val BRIGHT_YELLOW  = "\u001b[93m"
  final val BRIGHT_BLUE    = "\u001b[94m"
  final val BRIGHT_MAGENTA = "\u001b[95m"
  final val BRIGHT_CYAN    = "\u001b[96m"
  final val BRIGHT_WHITE   = "\u001b[97m"

  final val BOLD_BRIGHT_BLACK   = "\u001b[90;1m"
  final val BOLD_BRIGHT_RED     = "\u001b[91;1m"
  final val BOLD_BRIGHT_GREEN   = "\u001b[92;1m"
  final val BOLD_BRIGHT_YELLOW  = "\u001b[93;1m"
  final val BOLD_BRIGHT_BLUE    = "\u001b[94;1m"
  final val BOLD_BRIGHT_MAGENTA = "\u001b[95;1m"
  final val BOLD_BRIGHT_CYAN    = "\u001b[96;1m"
  final val BOLD_BRIGHT_WHITE   = "\u001b[97;1m"

  object Poison
  type Poison = Poison.type

  private[testutil] val printMutex = new AnyRef

  def lead(s: String) = s"$RED_B$s$RESET "

  def descMethod(method: String, desc: Option[String]): String =
    s"$method${desc.fold("")("(" + _ + ")")}"

  def failureStart(name: Option[String], leadSize: Int): Unit = {
    println()
    name.foreach(n => println(lead(">" * leadSize) + BRIGHT_YELLOW + n + RESET))
  }

  def printFailEA(name: Option[String], actual: Any, expect: Any): Unit =
    printFail2(name)("expect", BOLD_BRIGHT_GREEN, expect)("actual", BOLD_BRIGHT_RED, actual)

  def printFail2(name: Option[String])
                (title1: String, colour1: String, value1: Any)
                (title2: String, colour2: String, value2: Any): Unit = {

    val titleLen = title1.length max title2.length
    val leadFmt = s"%${titleLen}s:"

    failureStart(name, titleLen + 1)

    val toString: Any => String = {
      case s: Stream[_] => s.force.toString() // SI-9266
      case a            => a.toString
    }

    val show1 = toString(value1)
    val show2 = toString(value2)
    val ss = show2 :: show1 :: Nil
    var pre = "["
    var post = "]"
    val htChars = ss.flatMap(s => s.headOption :: s.lastOption :: Nil)
    if (htChars.forall(_.exists(c => !Character.isWhitespace(c)))) {
      pre = ""
      post = ""
    }
    if (ss.exists(_ contains "\n")) {
      pre = "↙[\n"
    }
    println(lead(leadFmt.format(title1)) + pre + colour1 + show1 + RESET + post)
    println(lead(leadFmt.format(title2)) + pre + colour2 + show2 + RESET + post)
  }

  def addSrcHint(msg: String)(implicit q: Line): String =
    s"$msg [L${q.value}]"

  def quoteStringForDisplay(s: String): String = {
    val sb = new StringBuilder
    sb append '⟪'
    s foreach {
      case '\b' => sb append '\\'; sb append 'b'
      case '\f' => sb append '\\'; sb append 'f'
      case '\n' => sb append '\\'; sb append 'n'
      case '\r' => sb append '\\'; sb append 'r'
      case '\t' => sb append '\\'; sb append 't'
      case '\\' => sb append '\\'; sb append '\\'
      case c    =>
        if (c >= ' ' && c <= '~')
          sb append c
        else {
          val hex = Integer.toHexString(c.toInt)
          sb append "\\u"
          hex.length match {
            case 1 => sb append "000"
            case 2 => sb append "00"
            case 3 => sb append '0'
            case _ =>
          }
          sb append hex
        }
    }
    sb append '⟫'
    sb.toString()
  }

}