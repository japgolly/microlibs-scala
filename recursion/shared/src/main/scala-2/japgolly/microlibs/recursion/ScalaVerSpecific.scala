package japgolly.microlibs.recursion

import cats.~>

object ScalaVerSpecific {
  private[recursion] type Coseq[F[_], G[_]] = Lambda[A => F[G[A]]] ~> Lambda[A => G[F[A]]]
}