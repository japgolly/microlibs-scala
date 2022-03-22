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

  implicit val i: Int = 123
  implicit val lli: List[List[Int]] = Nil :: Nil
  class I1(implicit j: Int)
  class I2[A](implicit a: A)
  class I3[A]()(implicit a: List[List[A]])
  newInstance[I1]
  newInstance[I2[Int]]
  // newInstance[I3[Int]]
  // try implicit defaults too

}