package japgolly.microlibs.utils

import japgolly.univeq.UnivEq
import scala.collection.compat._
import SafeBool._

/** Boolean isomorphism.
  *
  * Mix into the base type and override [[this.companion]] there.
  *
  * Examples:
  *
  * {{{
  *     sealed trait Enabled extends SafeBool[Enabled] {
  *       override final def companion = Enabled
  *     }
  *
  *     case object Enabled extends Enabled with SafeBool.Object[Enabled] {
  *       override def positive = Enabled
  *       override def negative = Disabled
  *     }
  *
  *     case object Disabled extends Enabled
  * }}}
  *
  * {{{
  *     sealed abstract class Permission extends SafeBool.WithBoolOps[Permission] {
  *       override final def companion = Permission
  *     }
  *
  *     case object Allow extends Permission
  *     case object Deny extends Permission
  *
  *     object Permission extends SafeBool.Object[Permission] {
  *       override def positive = Allow
  *       override def negative = Deny
  *     }
  * }}}
  */
trait SafeBool[B <: SafeBool[B]] extends Product with Serializable {
  this: B =>

  def companion: Object[B]

  final def unary_! : B =
    if (this == companion.positive)
      companion.negative
    else
      companion.positive

  @inline final def is(b: B): Boolean =
    b == this

  @inline final def when(cond: Boolean): B =
    if (cond) this else !this

  final def fnToThisWhen[A](f: A => Boolean): A => B =
    a => when(f(a))

  final def whenAllAre(bs: B*): B =
    this when bs.forall(is)

  final def whenAnyAre(bs: B*): B =
    this when bs.exists(is)
}

object SafeBool {

  /**
    * Mix into the companion object for the type.
    */
  trait Object[B <: SafeBool[B]] {
    implicit final def equality: UnivEq[B] = UnivEq.force

    def positive: B with SafeBool[B]
    def negative: B with SafeBool[B]

    final def memo[A](f: B => A): B => A = {
      val p = f(positive)
      val n = f(negative)
      b => if (b is positive) p else n
    }

    final def memoLazy[A](f: B => A): B => A = {
      lazy val p = f(positive)
      lazy val n = f(negative)
      b => if (b is positive) p else n
    }

    final def fold[A](a: A)(f: (A, B) => A): A =
      f(f(a, positive), negative)

    final def mapReduce[X, Y](m: B => X)(r: (X, X) => Y): Y =
      r(m(positive), m(negative))

    final def forall(f: B => Boolean): Boolean =
      f(positive) && f(negative)

    final def exists(f: B => Boolean): Boolean =
      f(positive) || f(negative)

    final type Values[+A] = SafeBool.Values[B, A]

    final object Values {
      def apply[A](f: B => A): Values[A] =
        SafeBool.Values(pos = f(positive), neg = f(negative))

      def both[A](a: A): Values[A] =
        SafeBool.Values(a, a)

      def partition[C[_], A](as: IterableOnce[A])(f: A => B)(implicit factory: Factory[A, C[A]]): Values[C[A]] = {
        val b = new Values(factory.newBuilder, factory.newBuilder)
        for (a <- as.iterator) b(f(a)) += a
        b.map(_.result())
      }
    }
  }

  /**
    * Adds boolean ops with `companion.positive` being the equivalent of `true`.
    */
  trait WithBoolOps[B <: SafeBool[B]] extends SafeBool[B] {
    this: B =>

    final def &(that: => B): B = {
      val pos = companion.positive
      pos when ((this is pos) && (that is pos))
    }

    final def &&(that: => Boolean): B = {
      val pos = companion.positive
      pos when ((this is pos) && that)
    }

    final def |(that: => B): B = {
      val pos = companion.positive
      pos when ((this is pos) || (that is pos))
    }

    final def ||(that: => Boolean): B = {
      val pos = companion.positive
      pos when ((this is pos) || that)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class Values[B <: SafeBool[B], +A](pos: A, neg: A) {
    def apply(b: B): A =
      if (b is b.companion.positive) pos else neg

    def set[AA >: A](b: B, a: AA): Values[B, AA] =
      if (b is b.companion.positive) copy(pos = a) else copy(neg = a)

    def mod[AA >: A](b: B, f: A => AA): Values[B, AA] =
      if (b is b.companion.positive) copy(pos = f(pos)) else copy(neg = f(neg))

    def map[C](f: A => C): Values[B, C] =
      Values(pos = f(pos), neg = f(neg))

    def ap[C, D](other: Values[B, C])(f: (A, C) => D): Values[B, D] =
      Values(pos = f(pos, other.pos), neg = f(neg, other.neg))

    def exists(f: A => Boolean): Boolean =
      f(pos) || f(neg)

    def forall(f: A => Boolean): Boolean =
      f(pos) && f(neg)
  }

  object Values {
    implicit def univEq[B <: SafeBool[B], A: UnivEq]: UnivEq[Values[B, A]] =
      UnivEq.derive
  }
}
