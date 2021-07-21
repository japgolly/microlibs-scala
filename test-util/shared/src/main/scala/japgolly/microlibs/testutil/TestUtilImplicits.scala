package japgolly.microlibs.testutil

import japgolly.microlibs.testutil.TestUtil.fail
import sourcecode.Line

trait TestUtilImplicits {
  import TestUtilImplicits._

  implicit def toTestUtilEitherExt[A, B](x: Either[A, B])(implicit l: Line): EitherExt[A, B] =
    new EitherExt(x)
}

object TestUtilImplicits {

  implicit class EitherExt[A, B](private val self: Either[A, B])(implicit l: Line) {

    def getOrThrow(): B =
      self.fold(e => fail(s"Expected Right(_), found Left($e)", clearStackTrace = false), identity)

    def getOrThrow(moreInfo: => String): B =
      self.fold(e => fail(s"${moreInfo.replaceFirst("\\.?$", ".")} Expected Right(_), found Left($e)"), identity)

    def getLeftOrThrow(): A =
      self.fold(identity, e => fail(s"Expected Left(_), found Right($e)", clearStackTrace = false))

    def getLeftOrThrow(moreInfo: => String): A =
      self.fold(identity, e => fail(s"${moreInfo.replaceFirst("\\.?$", ".")} Expected Left(_), found Right($e)"))
  }
}
