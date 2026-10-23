package japgolly.microlibs.stdlib_ext

import scala.collection.immutable.ArraySeq
import scala.collection.{Factory, View}
import scala.reflect.ClassTag

/**
  * Scala arrays don't support in-place modification.
  */
final class MutableArray[A](underlying: Array[Any]) {
  override def toString = underlying.mkString("MutableArray[", ", ", "]")

  inline def length = underlying.length
  inline def isEmpty = underlying.isEmpty
  inline def nonEmpty = underlying.nonEmpty

  private[this] var pendingMap: Option[Any => Any] = None

  private def applyPendingMap(): Unit =
    pendingMap.foreach { f =>
      pendingMap = None
      var i = length
      while (i > 0) {
        i -= 1
        underlying(i) = f(underlying(i))
      }
    }

  private def copyToPrimitive(dest: Array[A], ct: ClassTag[A]): Unit =
    ct match {
      case ClassTag.Int     => val d = dest.asInstanceOf[Array[Int]];     var i = length; while (i > 0) { i -= 1; d(i) = underlying(i).asInstanceOf[Int] }
      case ClassTag.Double  => val d = dest.asInstanceOf[Array[Double]];  var i = length; while (i > 0) { i -= 1; d(i) = underlying(i).asInstanceOf[Double] }
      case ClassTag.Long    => val d = dest.asInstanceOf[Array[Long]];    var i = length; while (i > 0) { i -= 1; d(i) = underlying(i).asInstanceOf[Long] }
      case ClassTag.Boolean => val d = dest.asInstanceOf[Array[Boolean]]; var i = length; while (i > 0) { i -= 1; d(i) = underlying(i).asInstanceOf[Boolean] }
      case ClassTag.Byte    => val d = dest.asInstanceOf[Array[Byte]];    var i = length; while (i > 0) { i -= 1; d(i) = underlying(i).asInstanceOf[Byte] }
      case ClassTag.Char    => val d = dest.asInstanceOf[Array[Char]];    var i = length; while (i > 0) { i -= 1; d(i) = underlying(i).asInstanceOf[Char] }
      case ClassTag.Float   => val d = dest.asInstanceOf[Array[Float]];   var i = length; while (i > 0) { i -= 1; d(i) = underlying(i).asInstanceOf[Float] }
      case ClassTag.Short   => val d = dest.asInstanceOf[Array[Short]];   var i = length; while (i > 0) { i -= 1; d(i) = underlying(i).asInstanceOf[Short] }
      case ClassTag.Unit    => val d = dest.asInstanceOf[Array[Unit]];    var i = length; while (i > 0) { i -= 1; d(i) = underlying(i).asInstanceOf[Unit] }
      case _                => var i = length; while (i > 0) { i -= 1; java.lang.reflect.Array.set(dest, i, underlying(i)) }
    }

  def array(implicit ct: ClassTag[A] = null): Array[A] = {
    applyPendingMap()
    if (ct == null || ct.runtimeClass.isAssignableFrom(underlying.getClass.getComponentType))
      underlying.asInstanceOf[Array[A]]
    else if (ct.runtimeClass.isPrimitive) {
      val dest = ct.newArray(length)
      copyToPrimitive(dest, ct)
      dest
    } else {
      val dest = ct.newArray(length)
      System.arraycopy(underlying, 0, dest, 0, length)
      dest
    }
  }

  inline def widen[B >: A]: MutableArray[B] =
    this.asInstanceOf[MutableArray[B]]

  def iterator(): Iterator[A] =
    pendingMap match {
      case None    => underlying.iterator.asInstanceOf[Iterator[A]]
      case Some(f) => underlying.iterator.map(f(_).asInstanceOf[A])
    }

  def map[B](f: A => B): MutableArray[B] = {
    val g = f.asInstanceOf[Any => Any]
    pendingMap = Some(pendingMap.fold(g)(g.compose))
    this.asInstanceOf[MutableArray[B]]
  }

  inline def sort(implicit o: Ordering[A]): MutableArray[A] = {
    scala.util.Sorting.quickSort(array(null))(o)
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

  inline def arraySeq(implicit ct: ClassTag[A] = null): ArraySeq[A] =
    ArraySeq.unsafeWrapArray(array(ct))

  inline def view: View[A] =
    View.fromIteratorProvider(() => iterator())

  inline def mkString(start: String, sep: String, end: String): String =
    iterator().mkString(start, sep, end)

  inline def mkString(sep: String): String =
    iterator().mkString(sep)

  inline def mkString: String =
    iterator().mkString
}

// =====================================================================================================================

object MutableArray {

  inline def apply[A](as: IterableOnce[A]): MutableArray[A] =
    new MutableArray(as.iterator.toArray[Any])

  inline def map[A, B](as: Iterable[A])(f: A => B): MutableArray[B] =
    apply(as.iterator.map(f))
}
