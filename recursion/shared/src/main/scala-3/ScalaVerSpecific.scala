package japgolly.microlibs.recursion

import cats.~>

object ScalaVerSpecific {

  private[recursion] type Coseq[F[_], G[_]] = ([A] =>> F[G[A]]) ~> ([A] =>> G[F[A]])
}