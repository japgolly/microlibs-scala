package japgolly.microlibs.testutil

import java.time.{Duration, Instant}
import utest._

object TestUtilTest extends TestSuite {
  import TestUtil._

  implicit class TestStringOps(private val s: String) extends AnyVal {
    def removeAnsiEscapeCodes: String =
      s.replaceAll("[\u001b\u009b][\\[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]", "")
  }

  private def captureOutput(a: => Any): String = {
    val os = new java.io.ByteArrayOutputStream()
    Console.withOut(os)(
      Console.withErr(os)(
        try a
        catch { case _: java.lang.AssertionError => () }
      )
    )
    os.toString
  }

  private def assertMultilineOutput(actual: String, expect: String, expectedOutput: String,
                                    cfg: AssertMultilineSettings => AssertMultilineSettings = identity): Unit = {
    val out = captureOutput(assertMultiline(actual, expect, cfg)).removeAnsiEscapeCodes.trim
    assertEq(out, expectedOutput.trim)
  }

  private def assertMultilineOutputNamed(name: String, actual: String, expect: String, expectedOutput: String): Unit = {
    val out = captureOutput(assertMultiline(name, actual, expect)).removeAnsiEscapeCodes.trim
    assertEq(out, expectedOutput.trim)
  }

  override def tests = Tests {

    "assertMultiline" - {
      "pass" - {
        assertMultiline("hello\nworld", "hello\nworld")
        assertMultiline("Named", "hello\nworld", "hello\nworld")
        assertMultiline("hello\nworld", "hello\nworld", _.copy(forceSideBySideDiff = true))
        assertMultiline("Named", "hello\nworld", "hello\nworld", _.copy(forceFlatDiff = true))
      }

      "emptyLines" - {
        // java.util.MissingFormatWidthException: %-0s
        val all = Seq("", " ", "\n", " \n ")
        for {
          a <- all
          b <- all
        } {
          try
            assertMultiline(a, b)
          catch {
            case _: java.lang.AssertionError => ()
          }
        }
      }

      "sideBySide" - {
        "diffIndicators" - {
          val actual = "a\nb \nc"
          val expect = "a\nb\nx"
          val expected =
            """assertMultiline (expect | actual)
              |1: a | | a
              |2: b |≈| b
              |3: x |≠| c""".stripMargin
          assertMultilineOutput(actual, expect, expected)
        }

        "headerIndicators" - {
          assertMultilineOutput("a", "a\nb",
            """assertMultiline (expect > actual)
              |  | a
              |-e| b
              |------------------------------------------------------------------------------------------------------------------------""".stripMargin)

          assertMultilineOutput("a\nb", "a",
            """assertMultiline (expect < actual)
              |  | a
              |+a| b
              |------------------------------------------------------------------------------------------------------------------------""".stripMargin)

          assertMultilineOutputNamed("CustomName", "a\nx", "a\ny",
            """assertMultiline: CustomName (expect | actual)
              |1: a | | a
              |2: y |≠| x""".stripMargin)
        }

        "sideBySideLongLeft" - {
          val expected =
            """assertMultiline (expect < actual)
              |1: a | | a
              |2: b | | b
              |3:   |≠| c""".stripMargin
          assertMultilineOutput("a\nb\nc", "a\nb", expected, _.copy(forceSideBySideDiff = true))
        }

        "sideBySideLongRight" - {
          val expected =
            """assertMultiline (expect > actual)
              |1: a | | a
              |2: b | | b
              |3: c |≠| """.stripMargin
          assertMultilineOutput("a\nb", "a\nb\nc", expected, _.copy(forceSideBySideDiff = true))
        }
      }

      "forceModes" - {
        "forceFlatDiff" - {
          val expected =
            s"""assertMultiline (expect | actual)
               |  | a
               |-e| y
               |+a| x
               |${"-" * 120}""".stripMargin
          assertMultilineOutput("a\nx", "a\ny", expected, _.copy(forceFlatDiff = true))
        }

        "forceSideBySideDiff" - {
          val expected =
            """assertMultiline (expect | actual)
              |1: a | | a
              |2: y |≠| x""".stripMargin
          assertMultilineOutput("a\nx", "a\ny", expected, _.copy(forceSideBySideDiff = true))
        }
      }

      "wrapping" - {
        val longA = "abcdefghij"
        val longE = "1234567890"

        val expectedWrap =
          """assertMultiline (expect | actual)
            |1: 1234 |≠| abcd
            |   5678 |≠| efgh
            |   90   |≠| ij""".stripMargin
        assertMultilineOutput(longA, longE, expectedWrap,
          _.copy(maxWidth = 16, sideBySideTruncateLongLinesOnDiff = false))

        val expectedTrunc =
          """assertMultiline (expect | actual)
            |1: 1234 |≠| abcd""".stripMargin
        assertMultilineOutput(longA, longE, expectedTrunc,
          _.copy(maxWidth = 16, sideBySideTruncateLongLinesOnDiff = true))
      }

      "globalSettings" - {
        val orig = TestUtilGlobalSettings.assertMultiline.value
        try {
          TestUtilGlobalSettings.assertMultiline.modify(_.copy(forceFlatDiff = true))
          val expected =
            s"""assertMultiline (expect | actual)
               |  | a
               |-e| y
               |+a| x
               |${"-" * 120}""".stripMargin
          val out = captureOutput(assertMultiline("a\nx", "a\ny")).removeAnsiEscapeCodes.trim
          assertEq(out, expected.trim)
        } finally {
          TestUtilGlobalSettings.assertMultiline.value = orig
        }
      }
    }


    "assertEqWithToleranceDouble" - {
      assertEqWithTolerance(10, 12, 2)
      assertEqWithTolerance(10, 12, 3)
      assertThrows(assertEqWithTolerance(10, 12, 1))
    }

    "assertEqWithToleranceInstant" - {
      val a = Instant.now()
      val b = a.plus(Duration.ofSeconds(2))
      assertEqWithTolerance(a, b, Duration.ofSeconds(2))
      assertEqWithTolerance(a, b, Duration.ofSeconds(3))
      assertThrows(assertEqWithTolerance(a, b, Duration.ofSeconds(1)))
    }

    "assertSeq" - {
      "leadFormat" - {
        val out = captureOutput(assertSeq(0 until 10, Seq.empty[Int]))
        assertContainsAll(out, "[0]")
        assertNotContainsAny(out, "[ 0]")
      }

      "noOutOfBoundsLoop" - {
        val out = captureOutput(assertSeq(Seq("a"), Seq("b")))
        assert(!out.contains("\n\n\n"))
      }
    }
  }
}
