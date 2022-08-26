package japgolly.microlibs.utils

import cats.instances.map._
import cats.syntax.foldable._
import cats.{Eq, Foldable}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq.UnivEq
import scala.annotation.{elidable, nowarn}
import scala.collection.IterableOnce

object IMapBaseV {
  def catsEq[K, V: Eq, M <: IMapBaseV[K, _, V, M]]: Eq[M] =
    Eq.by(_.underlyingMap)

  def univEq[K, VI, VO, I <: IMapBaseV[K, VI, VO, I]](implicit @nowarn("cat=unused") u: UnivEq[Map[K, VO]]): UnivEq[I] =
    UnivEq.force
}

abstract class IMapBaseV[K, VI, VO, This_ <: IMapBaseV[K, VI, VO, This_]] private[utils](m: Map[K, VO])(implicit @nowarn("cat=unused") u: UnivEq[K]) {
  final type This = This_
  final type M = Map[K, VO]

  override final def hashCode = m.##
  override final def equals(o: Any) = o match {
    case n: IMapBaseV[_, _, _, _] => m equals n.underlyingMap
    case n: Map[_, _]             => m equals n
    case _                        => false
  }

  override final def toString = {
    val s = m.toString
    if (s startsWith "Map")
      stringPrefix + s.drop(3)
    else
      s"$stringPrefix($s)"
  }

  protected def stringPrefix: String
  protected def setmap(n: M): This
  protected def _gkey(v: VI): K
  protected def _values(v: VO): IterableOnce[VI]
  protected def _add(to: M, k: K, v: VI): M

  final protected def __add(to: M, v: VI): M = _add(to, _gkey(v), v)

  @inline final def underlyingMap  = m
  @inline final def size           = m.size

  @inline final def iterator()      : Iterator[(K, VO)] = m.iterator
  @inline final def keys            : Iterable[K]       = m.keys
  @inline final def keysIterator()  : Iterator[K]       = m.keysIterator
  @inline final def keySet          : Set[K]            = m.keySet
  @inline final def values          : Iterable[VO]      = m.values
  @inline final def valuesIterator(): Iterator[VO]      = m.valuesIterator

  final def containsKey(k: K): Boolean =
    m.contains(k)

  final def containsValue(v: VI): Boolean =
    containsKey(_gkey(v))

  @inline final def mapValues[A](f: VO => A): Map[K, A] =
    m mapValuesNow f

  final def -(k: K) =
    setmap(m - k)

  @inline final def +(v: VI) = add(v)

  final def add(v: VI) =
    setmap(__add(m, v))

  final def addAll(vs: VI*) =
    addAllInFoldable(vs)

  final def addAllInFoldable[F[_]: Foldable](vs: F[VI]) =
    setmap(vs.foldLeft(m)(__add))

  final def ++(vs: IterableOnce[VI]) =
    setmap(vs.iterator.foldLeft(m)(__add))

  final def --(ks: IterableOnce[K]): This =
    setmap(m -- ks)

  @elidable(elidable.ASSERTION)
  final def assertValidKeys(map: M): Unit =
    for {
      (k1, vo) <- map
      v        <- _values(vo).iterator
    } assert(_gkey(v) == k1, s"Expected key for [$v] is [${_gkey(v)}] but [$k1] was found.")

  final def replaceUnderlying(map: M): This = {
    assertValidKeys(map)
    setmap(map)
  }

  final def mapUnderlying(f: M => M): This =
    replaceUnderlying(f(m))
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
