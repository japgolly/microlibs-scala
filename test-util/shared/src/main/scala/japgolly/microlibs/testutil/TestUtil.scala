package japgolly.microlibs.testutil

import scala.annotation.tailrec
import scalaz.Equal
import scalaz.syntax.equal._
import scala.io.AnsiColor._
import sourcecode.Line

private[testutil] object TestUtilColours {
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
}
import TestUtilColours._

object TestUtil extends TestUtil
trait TestUtil {

  def assertEq[A: Equal](actual: A, expect: A)(implicit q: Line): Unit =
    assertEqO(None, actual, expect)

  def assertEq[A: Equal](name: => String, actual: A, expect: A)(implicit q: Line): Unit =
    assertEqO(Some(name), actual, expect)

  private def lead(s: String) = s"$RED_B$s$RESET "
  private def failureStart(name: Option[String], leadSize: Int): Unit = {
    println()
    name.foreach(n => println(lead(">" * leadSize) + BRIGHT_YELLOW + n + RESET))
  }

  def assertEqO[A: Equal](name: => Option[String], actual: A, expect: A)(implicit q: Line): Unit =
    if (actual ≠ expect)
      fail2("assertEq", name)("expect", BOLD_BRIGHT_GREEN, expect)("actual", BOLD_BRIGHT_RED, actual)

  private def printFailEA(name: Option[String], actual: Any, expect: Any): Unit =
    printFail2(name)("expect", BOLD_BRIGHT_GREEN, expect)("actual", BOLD_BRIGHT_RED, actual)

  private def printFail2(name: Option[String])
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

  private def fail2(method: String, name: Option[String])
                   (title1: String, colour1: String, value1: Any)
                   (title2: String, colour2: String, value2: Any)
                   (implicit q: Line): Unit = {
    printFail2(name)(title1, colour1, value1)(title2, colour2, value2)
    println()
    fail(s"$method${name.fold("")("(" + _ + ")")} failed.")
  }

  private def addSrcHint(msg: String)(implicit q: Line): String =
    s"$msg [L${q.value}]"

  def assertNotEq[A: Equal](actual: A, expect: A)(implicit q: Line): Unit =
    assertNotEqO(None, actual, expect)

  def assertNotEq[A: Equal](name: => String, actual: A, expect: A)(implicit q: Line): Unit =
    assertNotEqO(Some(name), actual, expect)

  private def assertNotEqO[A: Equal](name: => Option[String], actual: A, expectNot: A)(implicit q: Line): Unit =
    if (actual === expectNot)
      fail2("assertNotEq", name)("expect not", BOLD_BRIGHT_BLUE, expectNot)("actual", BOLD_BRIGHT_RED, actual)

  def assertMultiline(actual: String, expect: String)(implicit q: Line): Unit =
    if (actual != expect) {
      println()
      val AE = List(actual, expect).map(_.split("\n"))
      val List(as, es) = AE
      val lim = as.length max es.length
      val List(maxA,_) = AE.map(x => (0 #:: x.map(_.length).toStream).max)
      val maxL = lim.toString.length
      println(s"${BRIGHT_YELLOW}assertMultiline:$RESET actual | expect")
      val fmtOK = s"${BRIGHT_BLACK}%${maxL}d: %-${maxA}s | | %s${RESET}\n"
      val fmtWS = s"${WHITE}%${maxL}d: ${RED_B}${BLACK}%-${maxA}s${RESET}${WHITE} |≈| ${GREEN_B}${BLACK}%s${RESET}\n"
      val fmtKO = s"${WHITE}%${maxL}d: ${BOLD_BRIGHT_RED}%-${maxA}s${RESET}${WHITE} |≠| ${BOLD_BRIGHT_GREEN}%s${RESET}\n"
      def removeWhitespace(s: String) = s.filterNot(_.isWhitespace)
      for (i <- 0 until lim) {
        val List(a, e) = AE.map(s => if (i >= s.length) "" else s(i))
        val fmt =
          if (a == e) fmtOK
          else if (removeWhitespace(a) == removeWhitespace(e)) fmtWS
          else fmtKO
        printf(fmt, i + 1, a, e)
      }
      println()
      fail("assertMultiline failed.")
    }

  def assertMap[K, V: Equal](actual: Map[K, V], expect: Map[K, V])(implicit q: Line): Unit =
    assertMapO(None, actual, expect)

  def assertMap[K, V: Equal](name: => String, actual: Map[K, V], expect: Map[K, V])(implicit q: Line): Unit =
    assertMapO(Some(name), actual, expect)

  def assertMapO[K, V: Equal](name: => Option[String], actual: Map[K, V], expect: Map[K, V])(implicit q: Line): Unit = {
    assertSet(name.fold("Map keys")(_ + " keys"), actual.keySet, expect.keySet)
    val bad = actual.keysIterator.filter(k => actual(k) ≠ expect(k))
    if (bad.nonEmpty) {
      val x = bad.toVector
      for (k <- x) {
        println(s"MapKey: $k")
        println(s"Expect: $BOLD_BRIGHT_GREEN${expect(k)}$RESET")
        println(s"Actual: $BOLD_BRIGHT_RED${actual(k)}$RESET")
        println()
      }
      fail(s"assertMap${name.fold("")("(" + _ + ")")} failed with ${x.length} value discrepancies.")
    }
  }

  def assertSeq[A: Equal](actual: Traversable[A])(expect: A*)(implicit q: Line): Unit = assertSeq(actual, expect.toSeq)
  def assertSeq[A: Equal](actual: Traversable[A], expect: Traversable[A])(implicit q: Line): Unit = assertSeqO(None, actual, expect)
  def assertSeq[A: Equal](name: => String, actual: Traversable[A])(expect: A*)(implicit q: Line): Unit = assertSeq(name, actual, expect.toSeq)
  def assertSeq[A: Equal](name: => String, actual: Traversable[A], expect: Traversable[A])(implicit q: Line): Unit = assertSeqO(Some(name), actual, expect)

  def assertSeqO[A: Equal](name: => Option[String], actual: Traversable[A], expect: Traversable[A])(implicit q: Line): Unit = {
    var failures = List.empty[Int]
    var lenOk    = true

    val ia = actual.toIterator
    val ie = expect.toIterator
    @tailrec def go(i: Int): Unit =
      if (ia.hasNext) {
        val a = ia.next()
        if (ie.hasNext) {
          val e = ie.next()
          if (a ≠ e)
            failures ::= i
          go(i + 1)
        } else
          lenOk = false
      } else
        lenOk = ie.isEmpty
    go(0)

    val pass = lenOk && failures.isEmpty
    if (!pass) {
      val av = actual.toVector
      val ev = expect.toVector
      val n = name.fold("")(_ + ": ")
      if (!lenOk) printFailEA(Some(n + "size"), actual = av.length, expect = ev.length)
      for (i <- failures.reverse) {
        val a = av(i)
        val e = ev(i)
        printFailEA(Some(s"${n}element ($i)"), actual = a, expect = e)
      }
      fail(s"assertSeq${name.fold("")("(" + _ + ")")} failed.")
    }
  }

  def assertSet[A](actual: Set[A])(expect: A*)(implicit q: Line): Unit = assertSet(actual, expect.toSet)
  def assertSet[A](actual: Set[A], expect: Set[A])(implicit q: Line): Unit = assertSetO(None, actual, expect)
  def assertSet[A](name: => String, actual: Set[A])(expect: A*)(implicit q: Line): Unit = assertSet(name, actual, expect.toSet)
  def assertSet[A](name: => String, actual: Set[A], expect: Set[A])(implicit q: Line): Unit = assertSetO(Some(name), actual, expect)

  def assertSetO[A](name: => Option[String], actual: Set[A], expect: Set[A])(implicit q: Line): Unit =
    if (actual != expect) {
      val missing = expect -- actual
      val unexpected = actual -- expect

      val leadSize = 9
      //if (missing.nonEmpty || unexpected.nonEmpty)
      //fail(s"Actual: $actual\nExpect: $expect\n   Missing: $missing\nUnexpected: $unexpected")
      def show(title: String, col: String, s: Set[A]): Unit =
        if (s.nonEmpty) {
          //val x = if (s.size == 1) s.head.toString else s.mkString("{ ",", "," }")
          val x = s.iterator.map(_.toString).toVector.sorted.mkString("\n" + (" " * (leadSize + 1)))
          println(lead(title) + col + x + RESET)
        }

      failureStart(name, leadSize)
      show(" missing:", BRIGHT_CYAN, missing)
      show("unwanted:", BOLD_BRIGHT_RED, unexpected)
      println()
      fail(s"assertSet${name.fold("")("(" + _ + ")")} failed.")
    }

  def fail(msg: String, clearStackTrace: Boolean = true, addSrcHint: Boolean = true)(implicit q: Line): Nothing = {
    val m = if (addSrcHint) this.addSrcHint(msg) else msg
    val e = new java.lang.AssertionError(m)
    if (clearStackTrace)
      e.setStackTrace(Array.empty)
    throw e
  }

  def assertContainsCI(actual: String, substr: String)(implicit q: Line): Unit =
    _assertContains("assertContainsCI", actual.toLowerCase, substr.toLowerCase, true)

  def assertContains(actual: String, substr: String)(implicit q: Line): Unit =
    _assertContains("assertContains", actual, substr, true)

  def assertNotContainsCI(actual: String, substr: String)(implicit q: Line): Unit =
    _assertContains("assertNotContainsCI", actual.toLowerCase, substr.toLowerCase, false)

  def assertNotContains(actual: String, substr: String)(implicit q: Line): Unit =
    _assertContains("assertNotContains", actual, substr, false)

  private def _assertContains(method: String, actual: String, substr: String, expect: Boolean)(implicit q: Line): Unit =
    if (actual.contains(substr) != expect) {
      printFail2(Some(method))(
        "substr", if (expect) BRIGHT_CYAN else BOLD_BRIGHT_BLUE, substr)(
        "actual", BOLD_BRIGHT_RED, actual)
      fail(s"$method failed.")
    }

  def assertChange[A, B: Equal, R](query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B)(implicit q: Line): R =
    assertChangeO(None, query, block)(actual)(expect)

  def assertChange[A, B: Equal, R](desc: => String, query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B)(implicit q: Line): R =
    assertChangeO(Some(desc), query, block)(actual)(expect)

  def assertChangeO[A, B: Equal, R](desc: => Option[String], query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B)(implicit q: Line): R = {
    val before = query
    val result = block
    val after  = query
    assertEqO(desc, actual(after, before), expect(before, result))
    result
  }

  def assertNoChange[B: Equal, A](query: => B)(block: => A)(implicit q: Line): A =
    assertNoChangeO(None, query)(block)

  def assertNoChange[B: Equal, A](desc: => String, query: => B)(block: => A)(implicit q: Line): A =
    assertNoChangeO(Some(desc), query)(block)

  def assertNoChangeO[B: Equal, A](desc: => Option[String], query: => B)(block: => A)(implicit q: Line): A =
    assertChangeO(desc, query, block)((b, _) => b)((b, _) => b)

  def assertDifference[N: Numeric : Equal, A](query: => N)(expect: N)(block: => A)(implicit q: Line): A =
    assertDifferenceO(None, query)(expect)(block)

  def assertDifference[N: Numeric : Equal, A](desc: => String, query: => N)(expect: N)(block: => A)(implicit q: Line): A =
    assertDifferenceO(Some(desc), query)(expect)(block)

  def assertDifferenceO[N: Numeric : Equal, A](desc: => Option[String], query: => N)(expect: N)(block: => A)(implicit q: Line): A =
    assertChangeO(desc, query, block)(implicitly[Numeric[N]].minus)((_, _) => expect)

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
