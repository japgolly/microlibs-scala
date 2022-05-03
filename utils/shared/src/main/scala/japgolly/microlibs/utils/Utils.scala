package japgolly.microlibs.utils

import cats.Order
import cats.kernel.Eq
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq.UnivEq
import java.time.{Duration, Instant}
import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.{ArraySeq, TreeMap}
import scala.collection.{Factory, Iterable}
import scala.reflect.ClassTag

object Utils {

  def arraySeqConcatDistinct[A](x: ArraySeq[A], y: ArraySeq[A])(implicit e: Eq[A]): ArraySeq[A] =
    if (x eq y)
      x
    else if (x.isEmpty)
      y
    else
      y.foldLeft(x)((q, a) =>
        if (x.exists(e.eqv(_, a))) q else q :+ a)

  def countOccurrences(str: String, subString: String): Int = {
    @tailrec def count(pos: Int, c: Int): Int = {
      val idx = str.indexOf(subString, pos)
      if (idx == -1)
        c
      else
        count(idx + subString.length, c + 1)
    }
    count(0, 0)
  }

  def cutoffStr(s: String, cutoff: Int): String =
    if (s.length <= cutoff)
      s
    else
      s.substring(0, cutoff - 1) + "\u2026"

  @nowarn("cat=unused")
  def dups[A: UnivEq](as: IterableOnce[A]): Iterator[A] = {
    val seen = collection.mutable.HashSet.empty[A]
    as.iterator.map { a =>
      if (seen contains a)
        Some(a)
      else {
        seen += a
        None
      }
    }.filterDefined
  }

  @nowarn("cat=unused")
  def enumOrdering[A: UnivEq, B: Ordering](as: IterableOnce[A])(by: A => B): Ordering[A] = {
    val sorted = MutableArray(as).sortBySchwartzian(by).array
    val ord = sorted.iterator.mapToOrder
    Ordering.by(ord.apply)
  }

  def filterAndSortByName[A](as: Iterable[A])(f: A => Boolean, name: A => String): Iterable[A] =
    as.foldLeft(TreeMap.empty[String, A])((q, a) =>
      if (f(a))
        q.updated(name(a), a)
      else
        q
    ).values

  @inline final def filterOutAndSortByName[A](as: Iterable[A])(f: A => Boolean, name: A => String): Iterable[A] =
    filterAndSortByName(as)(!f(_), name)

  /**
   * Fit an index within the acceptable window of indices.
   *
   * @param i The index needing correction.
   * @param length The length of the collection. Must be > 0.
   * @return 0 ≤ new-index < length
   */
  def fitCollectionIndex(i: Int, length: Int): Int = {
    assert(length > 0, "Length must be > 0, got: " + length)
    var j = i
    while (j >= length)
      j %= length
    while (j < 0)
      j += length
    j
  }

  def flattenArraySeqs[A: ClassTag](input: Seq[ArraySeq[A]]): ArraySeq[A] = {
    val as: IndexedSeq[ArraySeq[A]] =
      input match {
        case i: IndexedSeq[ArraySeq[A]] => i
        case _                          => input.to(ArraySeq)
      }
    as.length match {
      case 0 => ArraySeq.empty
      case 1 => as(0)
      case n =>

        val totalLen = {
          var len = 0
          var i = 0
          while (i < n) {
            val a = as(i)
            len += a.length
            i += 1
          }
          len
        }

        val result = new Array[A](totalLen)
        var offset = 0
        var i = 0
        while (i < n) {
          val a = as(i).unsafeArray.asInstanceOf[Array[A]]
          System.arraycopy(a, 0, result, offset, a.length)
          offset += a.length
          i += 1
        }
        ArraySeq.unsafeWrapArray(result)
    }
  }

  /**
    * Space = Θ(mn)
    * Time  = Θ(nᵐ)
    */
  def levenshtein(str1: String, str2: String): Int = {
    val m = str1.length
    val n = str2.length

    val d: Array[Array[Int]] = Array.ofDim(m + 1, n + 1)
    for (i <- 0 to m) d(i)(0) = i
    for (j <- 0 to n) d(0)(j) = j

    for (i <- 1 to m; j <- 1 to n) {
      val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1
      val a = d(i-1)(j  ) + 1     // deletion
      val b = d(i  )(j-1) + 1     // insertion
      val c = d(i-1)(j-1) + cost  // substitution
      d(i)(j) = a min b min c
    }

    d(m)(n)
  }

  def logTime[A](name: String)(a: => A): A = {
    println(s"Task [$name] started...")
    val start = Instant.now()
    try
      a
    finally {
      val end = Instant.now()
      val dur = Duration.between(start, end)
      println(s"Task [$name] completed in ${dur.conciseDesc}")
    }
  }

  def mergeDisjointMaps[K, V](x: Map[K, V], y: Map[K, V]): Map[K, V] =
         if (x.isEmpty) y
    else if (y.isEmpty) x
    else x ++ y

  def mergeMaps[K, V](x: Map[K, V], y: Map[K, V])(combine: (V, V) => V): Map[K, V] =
    if (x.isEmpty)
      y
    else if (y.isEmpty)
      x
    else
      x.foldLeft(y) { case (m, (k, v1)) =>
        val v =
          m.get(k) match {
            case None     => v1
            case Some(v2) => combine(v1, v2)
          }
        m.updated(k, v)
      }

  @nowarn("cat=unused")
  def mergeSets[A: UnivEq](x: Set[_ <: A], y: Set[_ <: A]): Set[A] =
         if (x.isEmpty) y.asInstanceOf[Set[A]]
    else if (y.isEmpty) x.asInstanceOf[Set[A]]
    else x ++ y

  def mergeSets[A: UnivEq](x: Set[_ <: A], y: Set[_ <: A], z: Set[_ <: A]): Set[A] =
         if (x.isEmpty) mergeSets(y, z)
    else if (y.isEmpty) mergeSets(x, z)
    else if (z.isEmpty) mergeSets(x, y)
    else (Set.newBuilder[A] ++= x ++= y ++= z).result()

  def mergeSets[A: UnivEq](w: Set[_ <: A], x: Set[_ <: A], y: Set[_ <: A], z: Set[_ <: A]): Set[A] =
         if (w.isEmpty) mergeSets(x, y, z)
    else if (x.isEmpty) mergeSets(w, y, z)
    else if (y.isEmpty) mergeSets(w, x, z)
    else if (z.isEmpty) mergeSets(w, x, y)
    else (Set.newBuilder[A] ++= w ++= x ++= y ++= z).result()

  def mergeSets[A: UnivEq](v: Set[_ <: A], w: Set[_ <: A], x: Set[_ <: A], y: Set[_ <: A], z: Set[_ <: A]): Set[A] =
         if (v.isEmpty) mergeSets(w, x, y, z)
    else if (w.isEmpty) mergeSets(v, x, y, z)
    else if (x.isEmpty) mergeSets(v, w, y, z)
    else if (y.isEmpty) mergeSets(v, w, x, z)
    else if (z.isEmpty) mergeSets(v, w, x, y)
    else (Set.newBuilder[A] ++= v ++= w ++= x ++= y ++= z).result()

  @nowarn("cat=unused")
  def nextElement[A: UnivEq](as: IndexedSeq[A])(a: A): A =
    as((as.indexOf(a) + 1) % as.length)

  def partitionBetween[F[x] <: Iterable[x], A](as: F[A])(split: (A, A) => Boolean)
                                              (implicit cbf: Factory[A, F[A]]): (F[A], F[A]) =
    if (as.isEmpty)
      (as, as)
    else {
      val b1, b2 = cbf.newBuilder
      val it = as.iterator
      @tailrec def go(prev: A): Unit =
        if (it.hasNext) {
          val a = it.next()
          if (split(prev, a)) {
            b2 += a
            b2 ++= it
          } else {
            b1 += a
            go(a)
          }
        }

      val first = it.next()
      b1 += first
      go(first)
      (b1.result(), b2.result())
    }

  def partitionConsecutive[F[x] <: Iterable[x], A](as: F[A])(implicit cbf: Factory[A, F[A]], n: Numeric[A]): (F[A], F[A]) =
    partitionConsecutiveBy(as)(identity)

  def partitionConsecutiveBy[F[x] <: Iterable[x], A, B](as: F[A])(f: A => B)
                                                       (implicit cbf: Factory[A, F[A]], n: Numeric[B]): (F[A], F[A]) =
    partitionBetween(as)((a, b) => !n.equiv(n.plus(f(a), n.one), f(b)))

  def quickStringExists(strings: Set[String]): String => Boolean = {
    val maxLen = strings.foldLeft(0)(_ max _.length)
    val byLen = Array.fill(maxLen + 1)(Set.empty[String])
    strings.foreach(s => byLen(s.length) = byLen(s.length) + s)
    s => (s.length <= maxLen) && byLen(s.length).contains(s)
  }

  // Improved a larger benchmark by 8.6%
  def quickStringLookup[A](map: Map[String, A]): String => Option[A] = {
    val strings = map.keySet
    val maxLen = strings.foldLeft(0)(_ max _.length)
    val byLen = Array.fill(maxLen + 1)(Map.empty[String, A])
    strings.foreach(s => byLen(s.length) = byLen(s.length).updated(s, map(s)))
    s => if (s.length <= maxLen) byLen(s.length).get(s) else None
  }

  /**
   * Pattern.quote doesn't work in Scala.JS.
   *
   * http://stackoverflow.com/questions/2593637/how-to-escape-regular-expression-in-javascript
   */
  def regexEscape(s: String): String = {
    var r = s
    r = regexEscape1.replaceAllIn(r, """\\$1""")
    r = regexEscape2.replaceAllIn(r, """\\x08""")
    r
  }

  private[this] val regexEscape1 = """([-()\[\]{}+?*.$\^|,:#<!\\])""".r
  private[this] val regexEscape2 = """\x08""".r

  def regexEscapeAndWrap(s: String): String =
    s"(?:${regexEscape(s)})"

  def separate(input: String, g: String => Int): Vector[Either[String, String]] = {
    val b = Vector.newBuilder[Either[String, String]]
    var i = 0
    var nongap = 0
    def takeNonGap(): Unit =
      if (nongap != 0) {
        val ng = input.substring(i - nongap, i)
        b += Right(ng)
        nongap = 0
      }

    while (i < input.length) {
      val s = input.drop(i)
      val gapSize = g(s)
      if (gapSize == 0) {
        nongap += 1
        i += 1
      } else {
        takeNonGap()
        val gap = s.take(gapSize)
        b += Left(gap)
        i += gapSize
      }
    }
    takeNonGap()
    b.result()
  }

  def separateByWhitespaceOrCommas(input: String): Vector[Either[String, String]] =
    separate(input, _.takeWhile(c => c == ',' || c.isWhitespace).length)

  def sideBySideStrings(str1: String, str2: String, sep: String = " | "): String =
    sideBySideStringSeqs(
      ArraySeq unsafeWrapArray str1.split('\n'),
      ArraySeq unsafeWrapArray str2.split('\n'),
      sep)
      .mkString("\n")

  def sideBySideStringSeqs(vec1: IndexedSeq[String], vec2: IndexedSeq[String], sep: String = " | "): Vector[String] = {
    def get(x: IndexedSeq[String], i: Int): String =
      if (i < x.length) x(i) else ""
    val fmt = s"%-${(vec1 :+ "").iterator.map(_.length).max}s%s%s"
    val b = Vector.newBuilder[String]
    for (i <- 0 until vec1.length.max(vec2.length)) {
      val x = get(vec1, i)
      val y = get(vec2, i)
      b += fmt.format(x, sep, y)
    }
    b.result()
  }

  @nowarn("cat=unused")
  def uniqueDupsNested[A, B: UnivEq](as: IterableOnce[A])(bs: A => IterableOnce[B]): Set[B] = {
    var uniq = Set.empty[B]
    var dups = Set.empty[B]
    for {
      a <- as.iterator
      b <- bs(a).iterator
    }
      if (!uniq.contains(b))
        // first sighting
        uniq += b
      else if (!dups.contains(b)) {
        // second sighting
        dups += b
      }
    dups
  }

  def univEqAndArbitraryOrder[A](values: Iterable[A]): UnivEq[A] with Order[A] = {
    val fixedOrder = values.zipWithIndex.toMap
    new UnivEq[A] with Order[A] {
      @inline private[this] def int(s: A) = fixedOrder(s)
      override def compare(a: A, b: A) = Order[Int].compare(int(a), int(b))
    }
  }

  def vectorConcatDistinct[A](x: Vector[A], y: Vector[A])(implicit e: Eq[A]): Vector[A] =
    if (x eq y)
      x
    else if (x.isEmpty)
      y
    else
      y.foldLeft(x)((q, a) =>
        if (x.exists(e.eqv(_, a))) q else q :+ a)
}
