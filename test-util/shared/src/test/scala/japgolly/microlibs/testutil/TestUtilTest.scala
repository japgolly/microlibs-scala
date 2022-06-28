package japgolly.microlibs.testutil

import utest._

object TestUtilTest extends TestSuite {
  import TestUtil._

  override def tests = Tests {

    "assertMultiline" - {

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

      "sideBySideLongLeft" -
        assertDiffReported(
          assertMultiline("a\nb\nc", "a\nb", _.copy(forceSideBySideDiff = true)))

      "sideBySideLongRight" -
        assertDiffReported(
          assertMultiline("a\nb", "a\nb\nc", _.copy(forceSideBySideDiff = true)))
    }
  }

  private def assertDiffReported(a: => Any): Unit = {
    val e = assertError(a)
    assertContains("" + e.getMessage, "assertMultiline failed")
  }
}
