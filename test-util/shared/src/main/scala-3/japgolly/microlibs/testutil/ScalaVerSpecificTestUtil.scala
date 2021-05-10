package japgolly.microlibs.testutil

import japgolly.microlibs.compiletime.InlineUtils
import japgolly.microlibs.compiletime.MacroEnv.*
import scala.quoted.*

// Scala 3

object ScalaVerSpecificTestUtil {

  def assertExprsEq[A](actual: Expr[A], expect: Expr[A], show: Boolean | Expr[Boolean] = false)(using Quotes): Expr[Unit] =
    if (actual matches expect) {
      val showValue: Boolean =
        show match {
          case b: Boolean       => b
          case e: Expr[Boolean] => e.value.getOrElse(false)
        }
      if showValue then println(actual.show)
      '{ () }
    } else {
      val a = Expr.inlineConst(actual.show)
      val e = Expr.inlineConst(expect.show)
      '{ TestUtil.assertMultiline($a, $e) }
    }

}

trait ScalaVerSpecificTestUtil {
  import TestUtil.*

  export InlineUtils.{
    showCode,
    showTasty,
  }

  inline def printCode(inline body: Any): body.type = {
    println(showCode(body))
    body
  }

  inline def printTasty(inline body: Any): body.type = {
    println(showTasty(body))
    body
  }

  inline def assertTastyEq[A](inline actual: A, inline expect: A, inline show: Boolean = false): Unit =
    ${ ScalaVerSpecificTestUtil.assertExprsEq[A](
      actual = 'actual,
      expect = 'expect,
      show   = 'show,
    ) }
}
