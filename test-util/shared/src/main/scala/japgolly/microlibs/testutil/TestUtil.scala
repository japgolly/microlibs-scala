package japgolly.microlibs.testutil

import cats.Eq
import cats.syntax.eq._
import japgolly.microlibs.testutil.TestUtilInternals._
import japgolly.univeq.UnivEq
import japgolly.univeq.UnivEqCats.catsEqFromUnivEq
import java.io.ByteArrayOutputStream
import scala.annotation._
import scala.io.AnsiColor._
import scala.util.{Failure, Success, Try}
import sourcecode.Line

trait TestUtilWithoutUnivEq
    extends ScalaVerSpecificTestUtil
       with TypeTestingUtil
       with TestUtilImplicits {

  import TestUtil._

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

  def failMethod(method: String)(implicit q: Line): Nothing =
    failMethod(method, None)

  def failMethod(method: String, desc: String)(implicit q: Line): Nothing =
    failMethod(method, Some(desc))

  def failMethod(method: String, desc: Option[String])(implicit q: Line): Nothing =
    fail(descMethod(method, desc) + " failed.")

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
                   (implicit q: Line): Nothing = {
    printFail2(name)(title1, colour1, value1)(title2, colour2, value2)
    println()
    failMethod(method, name)
  }

  def assertEq[A: Eq](actual: A, expect: A)(implicit q: Line): Unit =
    assertEqO(None, actual, expect)

  def assertEq[A: Eq](name: => String, actual: A, expect: A)(implicit q: Line): Unit =
    assertEqO(Some(name), actual, expect)

  def assertEqO[A: Eq](name: => Option[String], actual: A, expect: A)(implicit q: Line): Unit =
    if (actual =!= expect)
      fail2("assertEq", name)("expect", BOLD_BRIGHT_GREEN, expect)("actual", BOLD_BRIGHT_RED, actual)

  def assertNotEq[A: Eq](actual: A, expect: A)(implicit q: Line): Unit =
    assertNotEqO(None, actual, expect)

  def assertNotEq[A: Eq](name: => String, actual: A, expect: A)(implicit q: Line): Unit =
    assertNotEqO(Some(name), actual, expect)

  private def assertNotEqO[A: Eq](name: => Option[String], actual: A, expectNot: A)(implicit q: Line): Unit =
    if (actual === expectNot)
      fail2("assertNotEq", name)("expect not", BOLD_BRIGHT_BLUE, expectNot)("actual", BOLD_BRIGHT_RED, actual)

  def assertMultiline(actual: String, expect: String)(implicit q: Line): Unit =
    _assertMultiline(None, actual, expect, TestUtilGlobalSettings.assertMultiline.value)

  def assertMultiline(name: => String, actual: String, expect: String)(implicit q: Line): Unit =
    _assertMultiline(Some(name), actual, expect, TestUtilGlobalSettings.assertMultiline.value)

  def assertMultiline(actual: String, expect: String, cfg: AssertMultilineSettings)(implicit q: Line): Unit =
    _assertMultiline(None, actual, expect, cfg)

  def assertMultiline(name: => String, actual: String, expect: String, cfg: AssertMultilineSettings)(implicit q: Line): Unit =
    _assertMultiline(Some(name), actual, expect, cfg)

  def assertMultiline(actual: String, expect: String, cfg: AssertMultilineSettings => AssertMultilineSettings)(implicit q: Line): Unit =
    _assertMultiline(None, actual, expect, cfg(TestUtilGlobalSettings.assertMultiline.value))

  def assertMultiline(name: => String, actual: String, expect: String, cfg: AssertMultilineSettings => AssertMultilineSettings)(implicit q: Line): Unit =
    _assertMultiline(Some(name), actual, expect, cfg(TestUtilGlobalSettings.assertMultiline.value))

  private def _assertMultiline(name: => Option[String], actual: String, expect: String, cfg: AssertMultilineSettings)(implicit q: Line): Unit =
    if (actual != expect) withAtomicOutput {
      println()
      val EA = List(expect, actual).map(_.split("\n"))
      val List(es, as) = EA : @nowarn
      val lim = es.length max as.length
      val structureWidth = lim.toString.length + 7
      val List(maxAllE, _) = EA.map(x => (0 :: x.iterator.map(_.length).toList).max) : @nowarn
      val maxLimitE = (cfg.maxWidth - structureWidth) >> 1
      val maxE = maxAllE min maxLimitE
      val maxL = lim.toString.length
      if (maxL == 0 || maxE == 0)
        assertEqO(name, actual, expect)
      else {

        val nameSuffix = name.fold(RESET)(s":$RESET " + _)
        val fmtWSE = cfg.colourWhitespaceDiffExpect
        val fmtWSA = cfg.colourWhitespaceDiffActual
        val fmtKOE = cfg.colourDiffExpect
        val fmtKOA = cfg.colourDiffActual
        val cmp    = if (as.length == es.length) "|" else if (es.length > as.length) ">" else "<"
        println(s"${BRIGHT_YELLOW}assertMultiline$nameSuffix (${fmtKOE}expect$RESET $cmp ${fmtKOA}actual$RESET)")

        val sideBySide =
          (cfg.forceFlatDiff, cfg.forceSideBySideDiff, as.length == es.length) match {
            case (false, true, _) => true
            case (true, false, _) => false
            case (_, _, sameLens) => sameLens
          }

        if (sideBySide) {
          val fmtOK = s"${BRIGHT_BLACK}%${maxL}d: %-${maxE}s | | %s${RESET}\n"
          val fmtWS = s"${WHITE}%${maxL}d: ${fmtWSE}%-${maxE}s${RESET}${WHITE} |≈| ${fmtWSA}%s${RESET}\n"
          val fmtKO = s"${WHITE}%${maxL}d: ${fmtKOE}%-${maxE}s${RESET}${WHITE} |≠| ${fmtKOA}%s${RESET}\n"
          def removeWhitespace(s: String) = s.filterNot(_.isWhitespace)
          for (i <- 0 until lim) {
            val List(e, a) = EA.map(s => if (i >= s.length) "" else s(i)) : @nowarn

            val (fmt, truncate) =
              if (a == e)
                (fmtOK, cfg.sideBySideTruncateLongLinesOnMatch)
              else if (removeWhitespace(a) == removeWhitespace(e))
                (fmtWS, cfg.sideBySideTruncateLongLinesOnWhitespaceDiff)
              else
                (fmtKO, cfg.sideBySideTruncateLongLinesOnDiff)

            val l = i + 1
            val w = maxE
            @tailrec def go(x: String, y: String, fmt2: Option[String]): Unit = {
              fmt2 match {
                case None    => printf(fmt, l, x.take(w), y.take(w))
                case Some(f) => printf(f, x.take(w), y.take(w))
              }
              val hasMore = (x.length max y.length) > w
              if (hasMore && !truncate) {
                def newFmt = fmt.replace(s"%${maxL}d:", " " * (maxL + 1))
                go(x.drop(w), y.drop(w), fmt2.orElse(Some(newFmt)))
              }
            }
            go(e, a, None)
          }
        } else {
          println(LineDiff(expect, actual).expectActualColoured)
          println(BRIGHT_YELLOW + ("-" * 120) + RESET)
        }
        println()
        fail("assertMultiline failed.")
      }
    }

  def assertMap[K: UnivEq, V: Eq](actual: Map[K, V], expect: Map[K, V])(implicit q: Line): Unit =
    assertMapO(None, actual, expect)

  def assertMap[K: UnivEq, V: Eq](name: => String, actual: Map[K, V], expect: Map[K, V])(implicit q: Line): Unit =
    assertMapO(Some(name), actual, expect)

  def assertMapO[K: UnivEq, V: Eq](name: => Option[String], actual: Map[K, V], expect: Map[K, V])(implicit q: Line): Unit = {
    assertSet(name.fold("Map keys")(_ + " keys"), actual.keySet, expect.keySet)
    val bad = actual.keysIterator.filter(k => actual(k) =!= expect(k))
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

  def assertSeq[A: Eq](actual: Iterable[A])(expect: A*)(implicit q: Line): Unit = assertSeq(actual, expect.toSeq)
  def assertSeq[A: Eq](actual: Iterable[A], expect: Iterable[A])(implicit q: Line): Unit = assertSeqO(None, actual, expect)
  def assertSeq[A: Eq](name: => String, actual: Iterable[A])(expect: A*)(implicit q: Line): Unit = assertSeq(name, actual, expect.toSeq)
  def assertSeq[A: Eq](name: => String, actual: Iterable[A], expect: Iterable[A])(implicit q: Line): Unit = assertSeqO(Some(name), actual, expect)

  def assertSeqO[A: Eq](name: => Option[String], actual: Iterable[A], expect: Iterable[A])(implicit q: Line): Unit = {
    var failures = List.empty[Int]
    var lenOk    = true

    val ia = actual.iterator
    val ie = expect.iterator
    @tailrec def go(i: Int): Unit =
      if (ia.hasNext) {
        val a = ia.next()
        if (ie.hasNext) {
          val e = ie.next()
          if (a =!= e)
            failures ::= i
          go(i + 1)
        } else
          lenOk = false
      } else
        lenOk = ie.isEmpty
    go(0)

    val pass = lenOk && failures.isEmpty
    if (!pass) {
      val av         = actual.toVector
      val ev         = expect.toVector
      val lenMin     = av.length min ev.length
      val lenMax     = av.length max ev.length - 1
      val leadFmt    = s"[%${lenMax.toString.length}d]"
      val leadColour = RED_B
      val leadSize   = leadFmt.format(lenMax).length
      val leadBlank  = leadColour + (" " * leadSize)
      val n = name
      withAtomicOutput {
        failureStart(Some(descMethod("assertSeq", n)), leadSize + 8)

        var prevWasMultiline = false
        def log(i: Int): Unit = {
          val oa = av.lift(i)
          val oe = ev.lift(i)
          val lead = leadColour + leadFmt.format(i)
          def le(e: Any) = s" expect:$RESET ${BOLD_BRIGHT_GREEN}${e}$RESET"
          def la(a: Any) = s" actual:$RESET ${BOLD_BRIGHT_RED}${a}$RESET"
          if (prevWasMultiline) println()
          (oa, oe) match {

            case (Some(a), Some(e)) =>
              //val as = "" + a
              //val es = "" + e
              //if (as.length + es.length <= 110) {
              //  prevWasMultiline = false
              //  println(s"$lead noteql:$RESET ${BOLD_BRIGHT_GREEN}${es}$RESET ≠ ${BOLD_BRIGHT_RED}${as}$RESET")
              //} else {
                prevWasMultiline = true
                println(lead + le(e))
                println(leadBlank + la(a))
              //}

            case (Some(a), None) =>
              prevWasMultiline = false
              println(lead + la(a))

            case (None, Some(e)) =>
              prevWasMultiline = false
              println(lead + le(e))

            case (None, None) => ()
          }
        }

        failures.reverse.foreach(log)
        (lenMin to lenMax).foreach(log)
        println()
        fail(s"assertSeq${name.fold("")("(" + _ + ")")} failed.")
      }
    }
  }

  def assertSeqIgnoreOrder[A: Eq](actual: IterableOnce[A])(expect: A*)(implicit q: Line): Unit = assertSeqIgnoreOrder(actual, expect.toSeq)
  def assertSeqIgnoreOrder[A: Eq](actual: IterableOnce[A], expect: IterableOnce[A])(implicit q: Line): Unit = assertSeqIgnoreOrderO(None, actual, expect)
  def assertSeqIgnoreOrder[A: Eq](name: => String, actual: IterableOnce[A])(expect: A*)(implicit q: Line): Unit = assertSeqIgnoreOrder(name, actual, expect.toSeq)
  def assertSeqIgnoreOrder[A: Eq](name: => String, actual: IterableOnce[A], expect: IterableOnce[A])(implicit q: Line): Unit = assertSeqIgnoreOrderO(Some(name), actual, expect)

  def assertSeqIgnoreOrderO[A](name: => Option[String], actual: IterableOnce[A], expect: IterableOnce[A])
                              (implicit q: Line, A: Eq[A]): Unit =
    _assertSeqIgnoreOrderO("assertSeqIgnoreOrder")(name, actual, expect)

  private def _assertSeqIgnoreOrderO[A](methodName: String)
                                       (name: => Option[String], actual: IterableOnce[A], expect: IterableOnce[A])
                                       (implicit q: Line, A: Eq[A]): Unit = {
    val as = actual.iterator.toArray[Any]
    val es = expect.iterator.toArray[Any]
    var matches = 0

    for (ia <- as.indices) {
      val a = as(ia).asInstanceOf[A]
      @tailrec def go(ie: Int): Unit =
        if (ie >= 0) {
          val e = es(ie)
          val ok = !e.isInstanceOf[Poison] && A.eqv(a, e.asInstanceOf[A])
          if (ok) {
            matches += 1
            as(ia) = Poison
            es(ie) = Poison
          } else
            go(ie - 1)
        }
      go(es.length - 1)
    }

    val sizeMatch = as.length == es.length
    val pass      = sizeMatch && matches == es.length
    if (!pass) {
      val n = name
      // def prefix = n.fold("")(_ + ": ")
      def eTitle = "Expect elements:"
      def aTitle = "Actual elements:"
      // def eSizeT = "Expect seq size:"
      // def aSizeT = "Actual seq size:"

      withAtomicOutput {
        // if (sizeMatch)
        //   failureStart(n, aTitle.length)
        // else
        //   printFailEA(Some(prefix + "Element count"), actual = as.length, expect = es.length)
        failureStart(n, aTitle.length)
        // if (!sizeMatch) {
        //   println(s"${lead(eSizeT)}${BOLD_BRIGHT_GREEN}${es.length}$RESET")
        //   println(s"${lead(aSizeT)}${BOLD_BRIGHT_RED}${as.length}$RESET")
        // }

        def showElements(xs: Array[Any], title: String, colour: String, sizePrefix: String): Unit = {
          val b = Array.newBuilder[String]
          for (x <- xs)
            if (!x.isInstanceOf[Poison])
              b += "" + x
          val ss = b.result()
          if (ss.nonEmpty) {
            java.util.Arrays.sort(ss, Ordering[String])
            println(lead(title) + colour + sizePrefix + ss.length + " elements" + RESET)
            for (s <- ss)
              println(s"- $colour$s$RESET")
          }
        }

        showElements(es, eTitle, BOLD_BRIGHT_GREEN, "-")
        showElements(as, aTitle, BOLD_BRIGHT_RED, "+")
        println()
        failMethod(methodName, n)
      }
    }
  }

  def assertSet[A: UnivEq](actual: Set[A])(expect: A*)(implicit q: Line): Unit = assertSet(actual, expect.toSet)
  def assertSet[A: UnivEq](actual: Set[A], expect: Set[A])(implicit q: Line): Unit = assertSetO(None, actual, expect)
  def assertSet[A: UnivEq](name: => String, actual: Set[A])(expect: A*)(implicit q: Line): Unit = assertSet(name, actual, expect.toSet)
  def assertSet[A: UnivEq](name: => String, actual: Set[A], expect: Set[A])(implicit q: Line): Unit = assertSetO(Some(name), actual, expect)

  def assertSetO[A: UnivEq](name: => Option[String], actual: Set[A], expect: Set[A])(implicit q: Line): Unit =
    if (actual != expect)
      _assertSeqIgnoreOrderO("assertSet")(name, actual, expect)

  private def ci(s: String): String = s.toLowerCase
  private def ci(s: Set[String]): Set[String] = s.map(ci(_))

  def assertContains(actual: String, substr: String)(implicit q: Line): Unit =
    _assertContains("assertContains", actual, substr, true)

  def assertContainsCI(actual: String, substr: String)(implicit q: Line): Unit =
    _assertContains("assertContainsCI", ci(actual), ci(substr), true)

  def assertNotContains(actual: String, substr: String)(implicit q: Line): Unit =
    _assertContains("assertNotContains", actual, substr, false)

  def assertNotContainsCI(actual: String, substr: String)(implicit q: Line): Unit =
    _assertContains("assertNotContainsCI", ci(actual), ci(substr), false)

  private def _assertContains(method: String, actual: String, substr: String, expect: Boolean)(implicit q: Line): Unit =
    if (actual.contains(substr) != expect) {
      printFail2(Some(method))(
        "substr", _assertContainSubstrColour(expect), substr)(
        "actual", BOLD_BRIGHT_RED, actual)
      fail(s"$method failed.")
    }

  private def _assertContainSubstrColour(expect: Boolean): String =
    if (expect) BOLD_BRIGHT_GREEN else BOLD_BRIGHT_BLUE

  def assertContainsAny(actual: String, substrs: String*)(implicit q: Line): Unit =
    _assertContainsSet("assertContainsAny", actual, substrs.toSet, ∃ = true, expect = true)

  def assertContainsAll(actual: String, substrs: String*)(implicit q: Line): Unit =
    _assertContainsSet("assertContainsAll", actual, substrs.toSet, ∃ = false, expect = true)

  def assertNotContainsAny(actual: String, substrs: String*)(implicit q: Line): Unit =
    _assertContainsSet("assertNotContainsAny", actual, substrs.toSet, ∃ = true, expect = false)

  def assertNotContainsAll(actual: String, substrs: String*)(implicit q: Line): Unit =
    _assertContainsSet("assertNotContainsAll", actual, substrs.toSet, ∃ = false, expect = false)

  def assertContainsAnyCI(actual: String, substrs: String*)(implicit q: Line): Unit =
    _assertContainsSet("assertContainsAnyCI", ci(actual), ci(substrs.toSet), ∃ = true, expect = true)

  def assertContainsAllCI(actual: String, substrs: String*)(implicit q: Line): Unit =
    _assertContainsSet("assertContainsAllCI", ci(actual), ci(substrs.toSet), ∃ = false, expect = true)

  def assertNotContainsAnyCI(actual: String, substrs: String*)(implicit q: Line): Unit =
    _assertContainsSet("assertNotContainsAnyCI", ci(actual), ci(substrs.toSet), ∃ = true, expect = false)

  def assertNotContainsAllCI(actual: String, substrs: String*)(implicit q: Line): Unit =
    _assertContainsSet("assertNotContainsAllCI", ci(actual), ci(substrs.toSet), ∃ = false, expect = false)

  private def _assertContainsSet(method: String, actual: String, substrs: Set[String], ∃ : Boolean, expect: Boolean)(implicit q: Line): Unit = {
    val result =
      if (∃)
        substrs.exists(actual.contains)
      else
        substrs.forall(actual.contains)
    if (result != expect) {
      def substrToLine(s: String) = {
        var l = s.replace("\n", "\\n")
        val limit = 100
        if (l.length > limit) l = l.take(limit) + "…"
        l
      }
      val substrDesc = substrs.map(substrToLine).toList.sorted.mkString("\n")
      printFail2(Some(method))(
        "substrs", _assertContainSubstrColour(expect), substrDesc)(
        "actual", BOLD_BRIGHT_RED, actual)
      fail(s"$method failed.")
    }
  }

  def assertChange[A, B: Eq, R](query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B)(implicit q: Line): R =
    assertChangeO(None, query, block)(actual)(expect)

  def assertChange[A, B: Eq, R](desc: => String, query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B)(implicit q: Line): R =
    assertChangeO(Some(desc), query, block)(actual)(expect)

  def assertChangeO[A, B: Eq, R](desc: => Option[String], query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B)(implicit q: Line): R = {
    val before = query
    val result = block
    val after  = query
    assertEqO(desc, actual(after, before), expect(before, result))
    result
  }

  def assertNoChange[B: Eq, A](query: => B)(block: => A)(implicit q: Line): A =
    assertNoChangeO(None, query)(block)

  def assertNoChange[B: Eq, A](desc: => String, query: => B)(block: => A)(implicit q: Line): A =
    assertNoChangeO(Some(desc), query)(block)

  def assertNoChangeO[B: Eq, A](desc: => Option[String], query: => B)(block: => A)(implicit q: Line): A =
    assertChangeO(desc, query, block)((b, _) => b)((b, _) => b)

  def assertDifference[N: Numeric : Eq, A](query: => N)(expect: N)(block: => A)(implicit q: Line): A =
    assertDifferenceO(None, query)(expect)(block)

  def assertDifference[N: Numeric : Eq, A](desc: => String, query: => N)(expect: N)(block: => A)(implicit q: Line): A =
    assertDifferenceO(Some(desc), query)(expect)(block)

  def assertDifferenceO[N: Numeric : Eq, A](desc: => Option[String], query: => N)(expect: N)(block: => A)(implicit q: Line): A =
    assertChangeO(desc, query, block)(implicitly[Numeric[N]].minus)((_, _) => expect)

  def assertEqWithTolerance(actual: Double, expect: Double)(implicit l: Line): Unit =
    _assertEqWithTolerance(None, actual, expect)

  def assertEqWithTolerance(name: => String, actual: Double, expect: Double)(implicit l: Line): Unit =
    _assertEqWithTolerance(Some(name), actual, expect)

  def assertEqWithTolerance(actual: Double, expect: Double, tolerance: Double)(implicit l: Line): Unit =
    _assertEqWithTolerance(None, actual, expect, tolerance)

  def assertEqWithTolerance(name: => String, actual: Double, expect: Double, tolerance: Double)(implicit l: Line): Unit =
    _assertEqWithTolerance(Some(name), actual, expect, tolerance)

  private def _assertEqWithTolerance(_name: => Option[String], actual: Double, expect: Double, tolerance: Double = 0.001)(implicit l: Line): Unit = {
    val d = Math.abs(actual - expect)
    if (d > tolerance) {
      val name = _name
      val titleSuffix = name.fold("")(n => s" ${BOLD_BRIGHT_YELLOW}$n$RESET")
      val errorPrefix = name.fold("")(n => s"[$n] ")
      println(
        s"""
           |${YELLOW_B}${BLACK}assertEqWithTolerance failed:$RESET$titleSuffix
           |${BOLD_BRIGHT_GREEN}expect: $expect$RESET
           |${BOLD_BRIGHT_RED}actual: $actual$RESET
           |${BOLD_BRIGHT_RED} delta: $d$RESET
           |$YELLOW   tol: $tolerance$RESET
           |""".stripMargin)
      fail(s"$errorPrefix$actual ≠ $expect by $d which exceeds tolerance of $tolerance")
    }
  }

  def assertError(body: => Any): Throwable =
    Try(body) match {
      case Failure(t) => t
      case Success(b) => fail("Error expected but code completed with result: " + b)
    }
}

trait TestUtil
  extends TestUtilWithoutUnivEq
     with japgolly.univeq.UnivEqExports
     with japgolly.univeq.UnivEqCats

object TestUtil extends TestUtil {

  // TODO: Make diffing logic configurable by moving into config class
  // TODO: Now I now why I keep finding the colours confusing! diff:red=del,green=add, assert:red=bad,green=good

  case class AssertMultilineSettings(
      colourDiffActual                           : String,
      colourDiffExpect                           : String,
      colourWhitespaceDiffActual                 : String,
      colourWhitespaceDiffExpect                 : String,
      forceFlatDiff                              : Boolean,
      forceSideBySideDiff                        : Boolean,
      maxWidth                                   : Int,
      sideBySideTruncateLongLinesOnDiff          : Boolean,
      sideBySideTruncateLongLinesOnMatch         : Boolean,
      sideBySideTruncateLongLinesOnWhitespaceDiff: Boolean,
  )

  object AssertMultilineSettings {
    val default = AssertMultilineSettings(
      colourDiffActual                            = BLACK_B + BOLD_BRIGHT_RED,
      colourDiffExpect                            = BLACK_B + BOLD_BRIGHT_GREEN,
      colourWhitespaceDiffActual                  = RED_B + BLACK,
      colourWhitespaceDiffExpect                  = GREEN_B + BLACK,
      forceFlatDiff                               = false,
      forceSideBySideDiff                         = false,
      maxWidth                                    = 212,
      sideBySideTruncateLongLinesOnDiff           = false,
      sideBySideTruncateLongLinesOnMatch          = true,
      sideBySideTruncateLongLinesOnWhitespaceDiff = true,
    )
  }

  object TestUtilGlobalSettings {
    class Ref[A](init: A) {
      var value: A = init
      def modify(f: A => A): A = {
        value = f(value)
        value
      }
    }

    val assertMultiline = new Ref(AssertMultilineSettings.default)
  }
}
