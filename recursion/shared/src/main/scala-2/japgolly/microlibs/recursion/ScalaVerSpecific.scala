package japgolly.microlibs.recursion

import scalaz.~>

object ScalaVerSpecific {
  private[recursion] type Coseq[F[_], G[_]] = Lambda[A => F[G[A]]] ~> Lambda[A => G[F[A]]]
}