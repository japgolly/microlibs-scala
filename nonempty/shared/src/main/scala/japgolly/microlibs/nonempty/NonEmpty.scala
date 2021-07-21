package japgolly.microlibs.nonempty

import cats.Eq
import japgolly.univeq.UnivEq
import scala.annotation.nowarn

/**
 * Type indicating that its value has been proven to be non-empty.
 */
final class NonEmpty[A] private[NonEmpty] (val value: A) extends AnyVal {
  override def toString = s"NonEmpty($value)"
}

object NonEmpty {

  @inline implicit def autoNonEmptyValue[A](n: NonEmpty[A]): A =
    n.value

  /*
   * scala> class X(val i: Int) extends AnyVal
   * defined class X
   *
   * scala> new X(3) == new X(3)
   * res0: Boolean = true
   */
  @nowarn("cat=unused")
  @inline implicit def nonEmptyUnivEq[A: UnivEq]: UnivEq[NonEmpty[A]] =
    UnivEq.force

  @inline def nonEmptyEqual[A](implicit e: Eq[A]): Eq[NonEmpty[A]] =
    Eq.by(_.value)

  @inline def force[A](a: A): NonEmpty[A] =
    new NonEmpty(a)

  @inline def apply[I, O](i: I)(implicit proof: Proof[I, O]): Option[O] =
    proof.tryProve(i)

  @inline def require_![I, O](i: I)(implicit proof: Proof[I, O]): O =
    NonEmpty(i) getOrElse sys.error(s"Data is empty: $i")

//  def disj[I, O](i: I)(implicit proof: Proof[I, O]): I \/ O =
//    NonEmpty(i).fold[I \/ O](-\/(i))(\/-.apply)

  // -------------------------------------------------------------------------------------------------------------------
  //  Proofs

  final case class Proof[I, O](tryProve: I => Option[O]) extends AnyVal

  type ProofMono[A] = Proof[A, NonEmpty[A]]

  trait ProofImplicitsLo {
    def testEmptiness[A](isEmpty: A => Boolean): ProofMono[A] =
      Proof(a => if (isEmpty(a)) None else Some(new NonEmpty(a)))

    implicit def proveTraversable[A <: Iterable[_]]: ProofMono[A] =
      testEmptiness(_.isEmpty)
  }

  object Proof extends ProofImplicitsLo {
    implicit def proveNES[A: UnivEq]: Proof[Set[A], NonEmptySet[A]] =
      Proof(NonEmptySet.option[A])

    implicit def proveNEV[A]: Proof[Vector[A], NonEmptyVector[A]] =
      Proof(NonEmptyVector.option[A])
  }
}