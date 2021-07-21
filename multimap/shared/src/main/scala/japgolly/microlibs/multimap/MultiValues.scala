package japgolly.microlibs.multimap

import scala.collection.immutable.ArraySeq

trait MultiValues[L[_]] {
  def empty[A]: L[A]
  def add1[A](a: L[A], b: A): L[A]
  def del1[A](a: L[A], b: A): L[A]
  def addn[A](a: L[A], b: L[A]): L[A]
  def deln[A](a: L[A], b: L[A]): L[A]
  def foldl[A, B](a: A, b: L[B])(f: (A, B) => A): A
  def foldr[A, B](a: A, b: L[B])(f: (A, B) => A): A
  def iterator[A](a: L[A]): Iterator[A]
  def isEmpty[A](a: L[A]): Boolean
}

object MultiValues {
  @inline def apply[L[_]](implicit F: MultiValues[L]): MultiValues[L] = F

  trait Commutative[L[_]]

  implicit object ListMultiValues extends MultiValues[List] {
    override def empty   [A]                                     = List.empty[A]
    override def add1    [A]  (a: List[A], b: A)                 = b :: a
    override def del1    [A]  (a: List[A], b: A)                 = a.filterNot(_ == b)
    override def addn    [A]  (a: List[A], b: List[A])           = b ::: a
    override def deln    [A]  (a: List[A], b: List[A])           = {val s = b.toSet; a filterNot s.contains}
    override def foldl   [A,B](a: A, b: List[B])(f: (A, B) => A) = b.foldLeft(a)(f)
    override def foldr   [A,B](a: A, b: List[B])(f: (A, B) => A) = b.foldRight(a)((x, y) => f(y, x))
    override def iterator[A]  (a: List[A])                       = a.iterator
    override def isEmpty [A]  (a: List[A])                       = a.isEmpty
  }

  implicit object SetMultiValues extends MultiValues[Set] with Commutative[Set] {
    override def empty   [A]                                    = Set.empty[A]
    override def add1    [A]  (a: Set[A], b: A)                 = a + b
    override def del1    [A]  (a: Set[A], b: A)                 = a - b
    override def addn    [A]  (a: Set[A], b: Set[A])            = a ++ b
    override def deln    [A]  (a: Set[A], b: Set[A])            = a -- b
    override def foldl   [A,B](a: A, b: Set[B])(f: (A, B) => A) = b.foldLeft(a)(f)
    override def foldr   [A,B](a: A, b: Set[B])(f: (A, B) => A) = b.foldRight(a)((x, y) => f(y, x))
    override def iterator[A]  (a: Set[A])                       = a.iterator
    override def isEmpty [A]  (a: Set[A])                       = a.isEmpty
  }

  implicit object VectorMultiValues extends MultiValues[Vector] {
    override def empty   [A]                                       = Vector.empty[A]
    override def add1    [A]  (a: Vector[A], b: A)                 = a :+ b
    override def del1    [A]  (a: Vector[A], b: A)                 = a.filterNot(_ == b)
    override def addn    [A]  (a: Vector[A], b: Vector[A])         = a ++ b
    override def deln    [A]  (a: Vector[A], b: Vector[A])         = {val s = b.toSet; a filterNot s.contains}
    override def foldl   [A,B](a: A, b: Vector[B])(f: (A, B) => A) = b.foldLeft(a)(f)
    override def foldr   [A,B](a: A, b: Vector[B])(f: (A, B) => A) = b.foldRight(a)((x, y) => f(y, x))
    override def iterator[A]  (a: Vector[A])                       = a.iterator
    override def isEmpty [A]  (a: Vector[A])                       = a.isEmpty
  }

  implicit object ArraySeqMultiValues extends MultiValues[ArraySeq] {
    override def empty   [A]                                         = ArraySeq.empty[Any].asInstanceOf[ArraySeq[A]]
    override def add1    [A]  (a: ArraySeq[A], b: A)                 = a :+ b
    override def del1    [A]  (a: ArraySeq[A], b: A)                 = a.filterNot(_ == b)
    override def addn    [A]  (a: ArraySeq[A], b: ArraySeq[A])       = a ++ b
    override def deln    [A]  (a: ArraySeq[A], b: ArraySeq[A])       = {val s = b.toSet; a filterNot s.contains}
    override def foldl   [A,B](a: A, b: ArraySeq[B])(f: (A, B) => A) = b.foldLeft(a)(f)
    override def foldr   [A,B](a: A, b: ArraySeq[B])(f: (A, B) => A) = b.foldRight(a)((x, y) => f(y, x))
    override def iterator[A]  (a: ArraySeq[A])                       = a.iterator
    override def isEmpty [A]  (a: ArraySeq[A])                       = a.isEmpty
  }

}
