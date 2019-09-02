package japgolly.microlibs.stdlib_ext

/*
  +------------+
  | Scala 2.12 |
  +------------+
*/

trait ScalaSpecificStdlibExt {
  import ScalaSpecificStdlibExt._

  @inline final implicit def JSLE12_TraversableOnce[A](as: TraversableOnce[A]) = new JSLE12_TraversableOnce(as)
  @inline final implicit def JSLE12_OptionObj         (self: Option.type)      = new JSLE12_OptionObj(self)
}

object ScalaSpecificStdlibExt {

  final class JSLE12_OptionObj(private val self: Option.type) extends AnyVal {
    def when[A](cond: Boolean)(a: => A): Option[A] =
      if (cond) Some(a) else None

    @inline def unless[A](cond: Boolean)(a: => A): Option[A] =
      when(!cond)(a)
  }

  final class JSLE12_TraversableOnce[A](private val as: TraversableOnce[A]) extends AnyVal {
    def minOption[B >: A: Ordering]: Option[A] =
      if (as.isEmpty) None else Some(as.min[B])

    def maxOption[B >: A: Ordering]: Option[A] =
      if (as.isEmpty) None else Some(as.max[B])

    @deprecated("Use minByOption", "2.0 (and Scala 2.13)")
    def minOptionBy[B: Ordering](f: A => B): Option[A] =
      minByOption(f)

    @deprecated("Use maxByOption", "2.0 (and Scala 2.13)")
    def maxOptionBy[B: Ordering](f: A => B): Option[A] =
      maxByOption(f)

    def minByOption[B: Ordering](f: A => B): Option[A] =
      if (as.isEmpty) None else Some(as.minBy(f))

    def maxByOption[B: Ordering](f: A => B): Option[A] =
      if (as.isEmpty) None else Some(as.maxBy(f))

  }

}
