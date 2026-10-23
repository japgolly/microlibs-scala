package japgolly.microlibs.cats_ext

import java.time.{Duration, Instant}
import utest._

object CatsUtilTest extends TestSuite {

  override def tests = Tests {
    "equalInstantWithTolerance" - {
      "zeroTolerance" - {
        val eq = CatsUtil.equalInstantWithTolerance(Duration.ZERO)
        val now = Instant.now()
        assert(eq.eqv(now, now))
        assert(!eq.eqv(now, now.plusNanos(1)))
        assert(!eq.eqv(now, now.minusNanos(1)))
      }

      "exactBoundary" - {
        val tol = Duration.ofSeconds(2)
        val eq = CatsUtil.equalInstantWithTolerance(tol)
        val now = Instant.now()
        assert(eq.eqv(now, now))
        assert(eq.eqv(now, now.plus(tol)))
        assert(eq.eqv(now, now.minus(tol)))
        assert(eq.eqv(now.plus(tol), now))
        assert(!eq.eqv(now, now.plus(tol).plusNanos(1)))
        assert(!eq.eqv(now, now.minus(tol).minusNanos(1)))
      }
    }
  }
}
