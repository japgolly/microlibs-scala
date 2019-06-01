package japgolly.microlibs.utils

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.univeq.UnivEq
import scala.reflect.ClassTag

/** Fast, efficient lookup functions for static data.
  *
  * Creation verifies key uniqueness and throws runtime exceptions on failure.
  */
object StaticLookupFn {

  def useArray[A >: Null : ClassTag](as: Traversable[(Int, A)]): ArrayDsl[A] =
    if (as.isEmpty)
      new ArrayDsl[A] with Dsl.EmptyBase[Int, A] {
        override def to[V >: Null : ClassTag](ok: A => V, ko: Int => V) = ko
      }
    else
      new ArrayDsl[A] {
        override def total =
          to(identity, keyFail)

        override def toOption =
          to(Some(_), _ => None)

        override def toEither[E](e: Int => E) =
          to(Right(_), i => Left(e(i)))

        override def to[V >: Null : ClassTag](ok: A => V, ko: Int => V): Int => V = {
          val array = mkArray(ok)
          i => if (i >= 0 && i < array.length) {
            val a = array(i)
            if (null != a) a else ko(i)
          } else
            ko(i)
        }

        private def mkArray[X >: Null : ClassTag](toX: A => X): Array[X] = {
          val len = as.toIterator.map(_._1).max + 1
          val aa = Array.fill[A](len)(null)
          val ax = Array.fill[X](len)(null)
          for ((i, a) <- as) {
            assert(i >= 0, s"Indices can't be negative. Found: $i")
            val a2 = aa(i)
            if (null != a2)
              fail(s"Duplicates for index $i: $a and $a2")
            aa(i) = a
            ax(i) = toX(a)
          }
          ax
        }

        override protected def iterator() = as.toIterator
      }

  def useArrayBy[A >: Null : ClassTag](as: Traversable[A])(key: A => Int): DslBase[Int, A] =
    useArray(as.map(a => (key(a), a)))

  trait ArrayDsl[A] extends DslBase[Int, A] {
    def to[V >: Null : ClassTag](ok: A => V, ko: Int => V): Int => V
  }

  // ===================================================================================================================

  def useMap[K: UnivEq, V](kvs: Traversable[(K, V)]): Dsl[K, V] =
    if (kvs.isEmpty)
      Dsl.empty
    else
      new Dsl[K, V] {
        override def toOption: K => Option[V] =
          if (kvs.size <= 4)
            super.toOption
          else
            mkMap(identity).get

        override def to[A](ok: V => A, ko: K => A): K => A = {
          val m = mkMap(ok)
          k => m.getOrElse(k, ko(k))
        }

        private def mkMap[X](toX: V => X): Map[K, X] = {
          var mv = Map.empty[K, V]
          var mx = Map.empty[K, X]
          for ((k, v) <- kvs)
            mv.get(k) match {
              case None     => mv = mv.updated(k, v); mx = mx.updated(k, toX(v))
              case Some(v2) => fail(s"Duplicates for $k: ${v2} and $v")
            }
          mx
        }

        override protected def iterator() = kvs.toIterator
      }

  def useMapBy[K: UnivEq, V](vs: Traversable[V])(k: V => K) =
    useMap(vs.map(v => k(v) -> v))


  // ===================================================================================================================

  trait DslBase[@specialized(Int) K, V] {
    def total: K => V
    def toOption: K => Option[V]
    def toEither[E](e: K => E): K => Either[E, V]

    def toEitherWithHelp[H, E](h: V => String, sep: String = ",")(e: (K, String) => E): K => Either[E, V] = {
      val help = MutableArray(iterator().map(_._2).map(h)).sort.mkString(sep)
      toEither(e(_, help))
    }

    protected def iterator(): Iterator[(K, V)]
  }

  trait Dsl[@specialized(Int) K, V] extends DslBase[K, V] {
    override def total: K => V =
      to(identity, keyFail)

    override def toOption: K => Option[V] =
      to(Some(_), _ => None)

    override def toEither[E](e: K => E): K => Either[E, V] =
      to(Right(_), k => Left(e(k)))

    def to[A](ok: V => A, ko: K => A): K => A
  }

  object Dsl {
    def empty[@specialized(Int) K, V]: Dsl[K, V] =
      new Dsl[K, V] with EmptyBase[K, V] {
        override def to[A](ok: V => A, ko: K => A) = ko
      }

    trait EmptyBase[@specialized(Int) K, V] extends DslBase[K, V] {
      override def total = keyFail
      override def toOption = _ => None
      override def toEither[E](e: K => E) = k => Left(e(k))
      override protected def iterator() = Iterator.empty
    }
  }

  private def assert(t: Boolean, e: => String): Unit =
    if (!t) fail(e)

  private def fail(e: String): Nothing =
    throw new ExceptionInInitializerError(e)

  private val keyFail: Any => Nothing =
    k => throw new NoSuchElementException("key not found: " + k)
}
