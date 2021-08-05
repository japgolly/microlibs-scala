package japgolly.microlibs.compiletime

import scala.quoted.*

object Scala3CompilationTestsHelpers {
  inline def newInstance[A]: A =
    ${ _newInstance[A] }

  private def _newInstance[A](using Quotes, Type[A]): Expr[A] =
    NewInstance.of[A]()
}

