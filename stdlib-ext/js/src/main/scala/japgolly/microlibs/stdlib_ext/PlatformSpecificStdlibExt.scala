package japgolly.microlibs.stdlib_ext

import scalajs.js

/*
  +----+
  | JS |
  +----+
*/

object PlatformSpecificStdlibExt {

  final implicit class JSLE_JsIteratorUndefOr[A](private val as: Iterator[js.UndefOr[A]]) extends AnyVal {
    def nextOptionU: js.UndefOr[A] =
      if (as.hasNext)
        as.next()
      else
        js.undefined

    def firstDefined: js.UndefOr[A] =
      as.filter(_.isDefined).nextOptionU

    def filterDefined: Iterator[A] =
      as.filter(_.isDefined).map(_.get)
  }

}

trait PlatformSpecificStdlibExt {
  import PlatformSpecificStdlibExt._

  final implicit def JSLE_JsIteratorUndefOr[A](as: Iterator[js.UndefOr[A]]): JSLE_JsIteratorUndefOr[A] =
    new JSLE_JsIteratorUndefOr(as)
}
