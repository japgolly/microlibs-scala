package japgolly.microlibs.utils

import java.util.concurrent.atomic.AtomicInteger

object IntIncrementer {

  trait Dsl[F[_]] {

    def apply(): F[Int] =
      apply(1)

    def apply(startAt: Int): F[Int] =
      apply(identity, startAt)

    def apply[A](f: Int => A): F[A] =
      apply(f, 1)

    def apply[A](f: Int => A, startAt: Int): F[A]
  }

  // ===================================================================================================================

  object threadUnsafe extends Dsl[Function0] {
    override def apply[A](f: Int => A, startAt: Int): () => A = {
      var prev = startAt
      () => {
        prev += 1
        f(prev)
      }
    }
  }

  object volatile extends Dsl[Function0] {
    override def apply[A](f: Int => A, startAt: Int): () => A = {
      @volatile var prev = startAt
      () => {
        prev += 1
        f(prev)
      }
    }
  }

  object cas extends Dsl[Function0] {
    override def apply[A](f: Int => A, startAt: Int): () => A = {
      val prev = new AtomicInteger(startAt)
      () => f(prev.incrementAndGet())
    }
  }

  def syncOn(lock: AnyRef): Dsl[Function0] =
    new Dsl[Function0] {
    override def apply[A](f: Int => A, startAt: Int): () => A = {
      var prev = startAt
      () => {
        val n = lock.synchronized { prev += 1; prev }
        f(n)
      }
    }
  }
}
