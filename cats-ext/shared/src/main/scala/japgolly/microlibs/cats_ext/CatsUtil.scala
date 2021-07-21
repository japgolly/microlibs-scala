package japgolly.microlibs.cats_ext

import cats.Eq
import java.time.{Duration, Instant}

object CatsUtil {

  def equalInstantWithTolerance(tolerance: Duration): Eq[Instant] =
    Eq((a, b) => {
      val d = Duration.between(b, a).abs()
      tolerance.compareTo(d) > 0
    })

}
