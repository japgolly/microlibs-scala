package japgolly.microlibs.adt_macros

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import utest._
import aa._
import AdtMacros._
import japgolly.univeq.UnivEq

object Scala2AdtMacroTest extends TestSuite {
  import AdtMacroTest._

  override def tests = Tests {

    "adtValuesManually" - {
      "simple" - {
//      's1i - assertOrderedNEV(MonoS1.ValuesM)(MonoS1.A)
//      's3i - assertOrderedNEV(MonoS3.ValuesM)(MonoS3.A, MonoS3.B, MonoS3.C)
      "s1" - assertOrderedNEV(adtValuesManually[MonoS1](MonoS1.A))(MonoS1.A)
      "s3" - assertOrderedNEV(adtValuesManually[MonoS3](MonoS3.A, MonoS3.B, MonoS3.C))(MonoS3.A, MonoS3.B, MonoS3.C)
      }
      "dupTypes" - {
        "ok" - assertOrderedNEV(adtValuesManually[MonoD2](MonoD2.A, MonoD2.B(true), MonoD2.B(false)))(MonoD2.A, MonoD2.B(true), MonoD2.B(false))
      }
      "dupValues" - {
        "S1" - assertFail(compileError("adtValuesManually[MonoS1](MonoS1.A, MonoS1.A)"))
        "S3" - assertFail(compileError("adtValuesManually[MonoS3](MonoS3.A, MonoS3.B, MonoS3.B, MonoS3.C)"))
      }
      "incomplete" - {
        "O" - assertFail(compileError("adtValuesManually[MonoS3](MonoS3.A, MonoS3.C)"))
        "C" - assertFail(compileError("adtValuesManually[MonoD2](MonoD2.A)"))
      }
    }

  }
}
