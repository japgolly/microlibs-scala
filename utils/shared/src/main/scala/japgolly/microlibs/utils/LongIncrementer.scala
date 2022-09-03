package japgolly.microlibs.utils

import java.util.concurrent.atomic.AtomicLong

object LongIncrementer {

  trait Dsl[F[_]] {

    def apply(): F[Long] =
      apply(1L)

    def apply(startAt: Long): F[Long] =
      apply(identity, startAt)

    def apply[A](f: Long => A): F[A] =
      apply(f, 1L)

    def apply[A](f: Long => A, startAt: Long): F[A]
  }

  // ===================================================================================================================

  object threadUnsafe extends Dsl[Function0] {
    override def apply[A](f: Long => A, startAt: Long): () => A = {
      var prev = startAt
      () => {
        prev += 1
        f(prev)
      }
    }
  }

  object volatile extends Dsl[Function0] {
    override def apply[A](f: Long => A, startAt: Long): () => A = {
      @volatile var prev = startAt
      () => {
        prev += 1
        f(prev)
      }
    }
  }

  object cas extends Dsl[Function0] {
    override def apply[A](f: Long => A, startAt: Long): () => A = {
      val prev = new AtomicLong(startAt)
      () => f(prev.incrementAndGet())
    }
  }

  def syncOn(lock: AnyRef): Dsl[Function0] =
    new Dsl[Function0] {
    override def apply[A](f: Long => A, startAt: Long): () => A = {
      var prev = startAt
      () => {
        val n = lock.synchronized { prev += 1; prev }
        f(n)
      }
    }
  }
}
