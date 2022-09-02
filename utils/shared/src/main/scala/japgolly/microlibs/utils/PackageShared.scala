package japgolly.microlibs.utils

import java.time.Instant

trait PackageShared {

  final type MutableClock = MutableFn0[Instant]
  object MutableClock extends MutableFn0.DslWithDefaultSelector[Instant](() => Instant.now())

}
