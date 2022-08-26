package japgolly.microlibs.utils

import cats.Eq
import japgolly.univeq.UnivEq

object IMapBase {
  @inline def catsEq[K, V: Eq, M <: IMapBase[K, V, M]]: Eq[M] =
    IMapBaseV.catsEq[K, V, M]

  @inline def univEq[K, V, I <: IMapBase[K, V, I]](implicit u: UnivEq[Map[K, V]]): UnivEq[I] =
    IMapBaseV.univEq[K, V, V, I]
}

abstract class IMapBase[K: UnivEq, V, This_ <: IMapBase[K, V, This_]] private[utils] (m: Map[K, V]) extends IMapBaseV[K, V, V, This_](m) {
  final override protected def _values(v: V) = v :: Nil
  final override protected def _add(to: Map[K, V], k: K, v: V) = to.updated(k, v)

  final def isEmpty = m.isEmpty
  final def nonEmpty = !isEmpty

  final def filter      (f: (K, V) => Boolean): This = mapUnderlying(_ filter f.tupled)
  final def filterKeys  (f: K      => Boolean): This = mapUnderlying(_ filter(kv => f(kv._1)))
  final def filterValues(f: V      => Boolean): This = mapUnderlying(_.filter(kv => f(kv._2)))

  final def filterNot      (f: (K, V) => Boolean): This = mapUnderlying(_ filterNot f.tupled)
  final def filterNotKeys  (f: K      => Boolean): This = mapUnderlying(_.filterNot(kv => f(kv._1)))
  final def filterNotValues(f: V      => Boolean): This = mapUnderlying(_.filterNot(kv => f(kv._2)))
}
