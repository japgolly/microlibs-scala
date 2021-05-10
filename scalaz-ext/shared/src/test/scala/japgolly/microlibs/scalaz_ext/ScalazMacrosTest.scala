package japgolly.microlibs.scalaz_ext

import scalaz.Equal
import scalaz.std.anyVal.intInstance
import utest._

object ScalazMacrosTest extends TestSuite {

  sealed abstract class X[+E, +A]
  object X {
    sealed abstract class N[+A] extends X[Nothing, A]
    final case class S[+A](a: A) extends N[A]
    final case class F[+E](e: E) extends X[E, Nothing]
    case object U extends N[Nothing]

    implicit def eS[A: Equal]: Equal[S[A]]   = ScalazMacros.deriveEqual
    implicit def eF[E: Equal]: Equal[F[E]]   = ScalazMacros.deriveEqual
    implicit def eU          : Equal[U.type] = ScalazMacros.deriveEqual
  }

  override def tests = Tests {
    // TODO Pending https://github.com/lampepfl/dotty/issues/11765
    // "X" - {
    //   ScalazMacros.deriveEqual[X[Int, Int]]
    //   ()
    // }

  }
}
