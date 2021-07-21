package japgolly.microlibs.disjunction

import scala.util.control.NonFatal

trait Exports {

  final type -\/[+A] = scala.util.Left[A, Nothing]
  final type \/-[+A] = scala.util.Right[Nothing, A]

  @scala.annotation.showAsInfix
  final type \/[+A, +B] = Either[A, B]

  @inline final def -\/[A](a: A): -\/[A] = Left(a)
  @inline final def \/-[A](a: A): \/-[A] = Right(a)
  @inline final def \/                   = Exports.\/

  @inline final implicit def implicitIgnoreLeftTypeOnRight[A, B, R](r: Right[A, R]): Right[B, R] =
    r.asInstanceOf[Right[B, R]]

  @inline final implicit def implicitIgnoreRightTypeOnLeft[A, B, L](r: Left[L, A]): Left[L, B] =
    r.asInstanceOf[Left[L, B]]

  @inline final implicit def implicitDisjEitherOps[E, A](a: Either[E, A]): Exports.EitherOps[E, A] =
    new Exports.EitherOps(a)
}

object Exports {

  object \/ {
    def fromTryCatchNonFatal[A](a: => A): Either[Throwable, A] =
      try Right(a) catch { case NonFatal(e) => Left(e) }

    def fromTryCatch[A](a: => A): Either[Throwable, A] =
      try Right(a) catch { case e: Throwable => Left(e) }
  }

  final class EitherOps[E, A](private val e: Either[E, A]) extends AnyVal {
    def leftMap[B](f: E => B): Either[B, A] =
      e match {
        case r: Right[E, A] => r.asInstanceOf[Right[B, A]]
        case Left(e)        => Left(f(e))
      }
  }

}
