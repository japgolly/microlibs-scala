package japgolly.microlibs.scalaz_ext

import java.time.{Duration, Instant}
import scalaz.Equal

object ScalazUtil {

  def equalInstantWithTolerance(tolerance: Duration): Equal[Instant] =
    Equal((a, b) => {
      val d = Duration.between(b, a).abs()
      tolerance.compareTo(d) > 0
    })

}
