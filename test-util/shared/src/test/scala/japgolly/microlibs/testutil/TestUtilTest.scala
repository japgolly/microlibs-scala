package japgolly.microlibs.testutil

import utest._

object TestUtilTest extends TestSuite {
  import TestUtil._

  override def tests = Tests {

    "assertMultiline" - {
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
  }
}
