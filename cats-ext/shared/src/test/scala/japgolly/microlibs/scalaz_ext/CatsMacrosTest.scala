package japgolly.microlibs.cats_ext

import cats.Eq
import scala.annotation.nowarn
import utest._

@nowarn("cat=unused")
object CatsMacrosTest extends TestSuite {

  sealed abstract class X[+E, +A]
  object X {
    sealed abstract class N[+A] extends X[Nothing, A]
    final case class S[+A](a: A) extends N[A]
    final case class F[+E](e: E) extends X[E, Nothing]
    case object U extends N[Nothing]

    implicit def eS[A: Eq]: Eq[S[A]]   = CatsMacros.deriveEq
    implicit def eF[E: Eq]: Eq[F[E]]   = CatsMacros.deriveEq
    implicit def eU       : Eq[U.type] = CatsMacros.deriveEq
  }

  override def tests = Tests {
    // TODO Pending https://github.com/lampepfl/dotty/issues/11765
    // "X" - {
    //   CatsMacros.deriveEqual[X[Int, Int]]
    //   ()
    // }

  }
}
