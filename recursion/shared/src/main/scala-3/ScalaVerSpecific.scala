package japgolly.microlibs.recursion

import scalaz.~>

object ScalaVerSpecific {

  private[recursion] type Coseq[F[_], G[_]] = ([A] =>> F[G[A]]) ~> ([A] =>> G[F[A]])
}