package japgolly.microlibs.compiletime

import Scala3CompilationTestsHelpers._

object Scala3CompilationTests {

  class X
  InlineUtils.printCode(new X)
  InlineUtils.printTasty(new X)
  newInstance[X]

  class P[A]
  class M[A] { newInstance[P[A]] }
  newInstance[P[Int]]

  type PI = P[Int]
  newInstance[PI]
}