package japgolly.microlibs.utils

import cats.Eq
import japgolly.microlibs.nonempty.NonEmpty
import japgolly.microlibs.utils.PotentialChange._
import japgolly.univeq._
import scala.annotation.nowarn

sealed abstract class PotentialChange[+E, +A] {
  final def foreach(f: A => Unit): Unit =
    this match {
      case Success(a)             => f(a)
      case Failure(_) | Unchanged => ()
    }

  final def map[B](f: A => B): PotentialChange[E, B] =
    flatMap(a => Success(f(a)))

  final def flatMap[B, EE >: E](f: A => PotentialChange[EE, B]): PotentialChange[EE, B] =
    this match {
      case Success(a)   => f(a)
      case x@Failure(_) => x
      case Unchanged    => Unchanged
    }

  final def mapFailure[F](f: E => F): PotentialChange[F, A] =
    flatMapFailure(e => Failure(f(e)))

  final def flatMapFailure[F, AA >: A](f: E => PotentialChange[F, AA]): PotentialChange[F, AA] =
    this match {
      case x@Success(_) => x
      case Failure(e)   => f(e)
      case Unchanged    => Unchanged
    }

  final def ap[EE >: E, AA >: A, B, C](fb: PotentialChange[EE, B])
                                      (f: (Option[AA], Option[B]) => Option[C]): PotentialChange[EE, C] =
    (this, fb) match {
      case (x: NonFailure[AA], y: NonFailure[B]) => PotentialChange.fromOption(f(x.getUpdate, y.getUpdate))
      case (f: Failure[EE], _)                   => f
      case (_, f: Failure[EE])                   => f
    }

  final def merge[EE >: E, AA >: A, B, C](fb: PotentialChange[EE, B])
                                         (originalA: => AA, originalB: => B)
                                         (f: (AA, B) => C): PotentialChange[EE, C] =
    ap(fb)((oa, ob) => Some(f(oa getOrElse originalA, ob getOrElse originalB)))

  final def getUpdate: Option[A] =
    this match {
      case Success(a)             => Some(a)
      case Failure(_) | Unchanged => None
    }

  final def getFailure: Option[E] =
    this match {
      case Success(_) | Unchanged => None
      case Failure(e)             => Some(e)
    }

  final def isFailure: Boolean =
    this match {
      case Success(_) | Unchanged => false
      case Failure(_)             => true
    }

  final def isSuccess: Boolean =
    this match {
      case Success(_)             => true
      case Failure(_) | Unchanged => false
    }

  final def isUnchanged: Boolean =
    this match {
      case Success(_) | Failure(_) => false
      case Unchanged               => true
    }

  @inline final def isChanged: Boolean =
    !isUnchanged

  /** [[Unchanged]] is considered valid. */
  final def validity: Validity =
    Invalid when isFailure

  final def filter(f: A => Boolean): PotentialChange[E, A] =
    ignore(!f(_))

  final def ignore(f: A => Boolean): PotentialChange[E, A] =
    this match {
      case Success(a) if !f(a) => Unchanged
      case _                   => this
    }

  final def ignoreValue[AA >: A](a: => AA)(implicit e: Eq[AA]): PotentialChange[E, A] =
    ignore(e.eqv(a, _))

  final def ignoreOption[AA >: A](o: => Option[AA])(implicit e: Eq[AA]): PotentialChange[E, A] =
    ignore(a => o.fold(false)(e.eqv(a, _)))

  final def ignoreEmpty[AA >: A, B](implicit p: NonEmpty.Proof[AA, B]): PotentialChange[E, B] =
    flatMap(PotentialChange.nonEmpty[AA, B](_)(p))

  final def setDiff[B](prev: Set[B])(implicit ev: A <:< Set[B], univEq: UnivEq[B]): PotentialChange[E, SetDiff.NE[B]] =
    flatMap(a => PotentialChange.fromOption(NonEmpty(SetDiff.compare(prev, ev(a)))))

  final def setDiffOption[B](prev: Option[Set[B]])(implicit ev: A <:< Set[B], univEq: UnivEq[B]): PotentialChange[E, SetDiff.NE[B]] =
    flatMap(a => PotentialChange.fromOption(NonEmpty(SetDiff.compareOption(prev, ev(a)))))

  final def toOption: Option[A] =
    this match {
      case Success(a)             => Some(a)
      case Failure(_) | Unchanged => None
    }

  final def toEitherOption: Either[E, Option[A]] =
    this match {
      case Success(a) => Right(Some(a))
      case Unchanged  => Right(None)
      case Failure(e) => Left(e)
    }
}

object PotentialChange {

  sealed trait NonFailure[+A] extends PotentialChange[Nothing, A] {
    final def foldNonFailure[B](changed: A => B, unchanged: => B): B =
      this match {
        case Success(a) => changed(a)
        case Unchanged  => unchanged
      }
  }

  sealed trait Changed[+E, +A] extends PotentialChange[E, A]

  case object Unchanged extends NonFailure[Nothing]

  final case class Success[+A](update: A) extends NonFailure[A] with Changed[Nothing, A]

  final case class Failure[+E](failure: E) extends Changed[E, Nothing]

  @nowarn("cat=unused")
  implicit def univEq[E: UnivEq, A: UnivEq]: UnivEq[PotentialChange[E, A]] =
    UnivEq.derive

  // no compare, compareOption etc because they're methods on instances instead

  def fromEither[E, A](d: Either[E, A]): PotentialChange[E, A] =
    d match {
      case Right(a) => Success(a)
      case Left(e)  => Failure(e)
    }

  def fromOption[A](o: Option[A]): NonFailure[A] =
    o match {
      case Some(a) => Success(a)
      case None    => Unchanged
    }

  def needFromOption[A](o: Option[A]): PotentialChange[Unit, A] =
    o match {
      case Some(a) => Success(a)
      case None    => Failure(())
    }

  def nonEmpty[A, B](a: A)(implicit p: NonEmpty.Proof[A, B]): NonFailure[B] =
    fromOption(p tryProve a)
}