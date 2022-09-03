package japgolly.microlibs.testutil

import java.time.{Duration, Instant}
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
  }
}
