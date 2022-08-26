package japgolly.microlibs.utils

import cats.Eq
import japgolly.microlibs.nonempty.NonEmpty
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.univeq.UnivEq

object IMap {
  @inline implicit def catsEq[K, V: Eq]: Eq[IMap[K, V]] =
    IMapBaseV.catsEq[K, V, IMap[K, V]]

  @inline implicit def univEq[K, V](implicit u: UnivEq[Map[K, V]]): UnivEq[IMap[K, V]] =
    IMapBaseV.univEq[K, V, V, IMap[K, V]](u)

  implicit def nonEmptyProof[K, V]: NonEmpty.ProofMono[IMap[K, V]] =
    NonEmpty.Proof.testEmptiness(_.isEmpty)

  def empty[K: UnivEq, V](k: V => K): IMap[K, V] =
    new IMap(k, Map.empty)
}

/**
 * Intrinsic-Invariant Map.
 *
 * A map with automatically-managed intrinsic-invariants.
 * The relationship between map-key and value is guaranteed to be consistent.
 *
 * Values are mapped by a subset of themselves.
 */
final class IMap[K: UnivEq, V] private (key: V => K, m: Map[K, V]) extends IMapBase[K, V, IMap[K, V]](m) {

  override protected def stringPrefix = "IMap"
  override protected def setmap(n: M) = new IMap(key, n)
  override protected def _gkey(v: V)  = key(v)

  def get(k: K): Option[V] =
    m.get(k)

  def need(k: K): V = {
    val o = get(k)
    if (o.isEmpty)
      throw new RuntimeException(badKeyMsg(k))
    else
      o.get
  }

  def getAttempt(k: K): Either[String, V] =
    get(k).toRight(badKeyMsg(k))

  private def badKeyMsg(k: K): String = {
    val keyArray = MutableArray(keysIterator().map(_.toString)).sort
    val max = 10
    val keyDesc =
      if (keyArray.length > max)
        keyArray.iterator().take(max).mkString("{", ", ", ", ... }")
      else
        keyArray.iterator().mkString("{", ", ", "}")
    s"Value not found for $k.\nKeys = $keyDesc"
  }

  def modifyValues(f: V => V): This =
    new IMap(key, m.valuesIterator.map(f).map(v => key(v) -> v).toMap)

  def modify(k: K, f: V => V)(implicit ev: V <:< AnyRef): This =
    _mod(k, f, this)

  def modifyOrPut(k: K, f: V => V, put: => V)(implicit ev: V <:< AnyRef): This =
    _mod(k, f, this add put)

  private def _mod(k: K, f: V => V, nomod: => IMap[K, V])(implicit ev: V <:< AnyRef): This =
    m.get(k).fold(nomod)(v => {
      val v2 = f(v)
      if (ev(v) eq ev(v2))
        this
      else {
        val k2 = key(v2)
        var n = m.updated(k2, v2)
        if (k != k2) n -= k
        setmap(n)
      }
    })

  def empty: This =
    IMap.empty(key)
}
