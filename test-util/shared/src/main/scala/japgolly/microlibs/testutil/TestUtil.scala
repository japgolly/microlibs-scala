package japgolly.microlibs.testutil

import java.io.ByteArrayOutputStream
import scala.annotation.tailrec
import scalaz.Equal
import scalaz.syntax.equal._
import scala.io.AnsiColor._
import sourcecode.Line
import TestUtilInternals._

object TestUtil extends TestUtil
trait TestUtil {

  def withAtomicOutput[A](a: => A): A = {
    val os = new ByteArrayOutputStream()
    try
      printMutex.synchronized(
        Console.withOut(os)(
          Console.withErr(os)(
            a)))
    finally
      print(os.toString())
  }

  def fail(msg: String, clearStackTrace: Boolean = true, addSrcHint: Boolean = true)(implicit q: Line): Nothing = {
    val m = if (addSrcHint) TestUtilInternals.addSrcHint(msg) else msg
    val e = new java.lang.AssertionError(m)
    if (clearStackTrace)
      e.setStackTrace(Array.empty)
    throw e
  }

  def onFail[A](body: => A)(onFail: => Any): A =
    try
      body
    catch {
      case t: java.lang.AssertionError =>
        onFail
        throw t
    }

  def onError[A](body: => A)(onError: Throwable => Any): A =
    try
      body
    catch {
      case t: Throwable =>
        onError(t)
        throw t
    }

  private def fail2(method: String, name: Option[String])
                   (title1: String, colour1: String, value1: Any)
                   (title2: String, colour2: String, value2: Any)
                   (implicit q: Line): Unit = {
    printFail2(name)(title1, colour1, value1)(title2, colour2, value2)
    println()
    fail(s"$method${name.fold("")("(" + _ + ")")} failed.")
  }

  def assertEq[A: Equal](actual: A, expect: A)(implicit q: Line): Unit =
    assertEqO(None, actual, expect)

  def assertEq[A: Equal](name: => String, actual: A, expect: A)(implicit q: Line): Unit =
    assertEqO(Some(name), actual, expect)

  def assertEqO[A: Equal](name: => Option[String], actual: A, expect: A)(implicit q: Line): Unit =
    if (actual ≠ expect)
      fail2("assertEq", name)("expect", BOLD_BRIGHT_GREEN, expect)("actual", BOLD_BRIGHT_RED, actual)

  def assertNotEq[A: Equal](actual: A, expect: A)(implicit q: Line): Unit =
    assertNotEqO(None, actual, expect)

  def assertNotEq[A: Equal](name: => String, actual: A, expect: A)(implicit q: Line): Unit =
    assertNotEqO(Some(name), actual, expect)

  private def assertNotEqO[A: Equal](name: => Option[String], actual: A, expectNot: A)(implicit q: Line): Unit =
    if (actual === expectNot)
      fail2("assertNotEq", name)("expect not", BOLD_BRIGHT_BLUE, expectNot)("actual", BOLD_BRIGHT_RED, actual)

  def assertMultiline(actual: String, expect: String)(implicit q: Line): Unit =
    if (actual != expect) withAtomicOutput {
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
      withAtomicOutput {
        if (!lenOk) printFailEA(Some(n + "size"), actual = av.length, expect = ev.length)
        for (i <- failures.reverse) {
          val a = av(i)
          val e = ev(i)
          printFailEA(Some(s"${n}element ($i)"), actual = a, expect = e)
        }
        fail(s"assertSeq${name.fold("")("(" + _ + ")")} failed.")
      }
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

      withAtomicOutput {
        failureStart(name, leadSize)
        show(" missing:", BRIGHT_CYAN, missing)
        show("unwanted:", BOLD_BRIGHT_RED, unexpected)
        println()
        fail(s"assertSet${name.fold("")("(" + _ + ")")} failed.")
      }
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

}
