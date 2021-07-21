package japgolly.microlibs.stdlib_ext

import scala.collection.immutable.ArraySeq
import scala.collection.{Factory, View}
import scala.collection.Factory

/**
  * Scala arrays don't support in-place modification.
  */
final class MutableArray[A](underlying: Array[Any]) {
  override def toString = underlying.mkString("MutableArray[", ", ", "]")

  inline def length = underlying.length
  inline def isEmpty = underlying.isEmpty
  inline def nonEmpty = underlying.nonEmpty

  private[this] var pendingMap: Option[Any => Any] = None

  def array: Array[A] = {
    pendingMap.foreach { f =>
      pendingMap = None
      var i = length
      while (i > 0) {
        i -= 1
        underlying(i) = f(underlying(i))
      }
    }
    underlying.asInstanceOf[Array[A]]
  }

  inline def widen[B >: A]: MutableArray[B] =
    this.asInstanceOf[MutableArray[B]]

  def iterator(): Iterator[A] =
    pendingMap match {
      case None    => array.iterator
      case Some(f) => underlying.iterator.map(f(_).asInstanceOf[A])
    }

  def map[B](f: A => B): MutableArray[B] = {
    val g = f.asInstanceOf[Any => Any]
    pendingMap = Some(pendingMap.fold(g)(g.compose))
    this.asInstanceOf[MutableArray[B]]
  }

  inline def sort(implicit o: Ordering[A]): MutableArray[A] = {
    scala.util.Sorting.quickSort(array)(o)
    this
  }

  inline def sortBy[B: Ordering](f: A => B): MutableArray[A] =
    sort(Ordering by f)

  def sortBySchwartzian[B: Ordering](f: A => B): MutableArray[A] =
    map(a => (f(a), a))
      .sort(Ordering.by((_: (B, A))._1))
      .map(_._2)

  def to[B](f: Factory[A, B]): B = {
    val b = f.newBuilder
    b.sizeHint(length)
    iterator().foreach(b += _)
    b.result()
  }

  inline def arraySeq: ArraySeq[A] =
    ArraySeq.unsafeWrapArray(array)

  inline def view: View[A] =
    View.fromIteratorProvider(() => iterator())

  inline def mkString(start: String, sep: String, end: String): String =
    array.mkString(start, sep, end)

  inline def mkString(sep: String): String =
    array.mkString(sep)

  inline def mkString: String =
    array.mkString
}

// =====================================================================================================================

object MutableArray {

  inline def apply[A](as: IterableOnce[A]): MutableArray[A] =
    new MutableArray(as.iterator.toArray[Any])

  inline def map[A, B](as: Iterable[A])(f: A => B): MutableArray[B] =
    apply(as.iterator.map(f))
}
