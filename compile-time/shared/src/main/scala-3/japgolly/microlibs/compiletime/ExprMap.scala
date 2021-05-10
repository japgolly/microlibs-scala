package japgolly.microlibs.compiletime

import scala.quoted.*

object ExprMap:
  def empty[K, V](using Quotes): ExprMap[K, V] =
    new ExprMap[K, V]

final class ExprMap[K, V](using Quotes):
  private var exprs = List.empty[(Expr[K], V)]

  def update(k: Expr[K], v: V): this.type =
    this += ((k, v))

  def +=(kv: (Expr[K], V)): this.type =
    this -= kv._1
    exprs ::= kv
    this

  def -=(k: Expr[K]): this.type =
    exprs = exprs.filterNot(x => k.matches(x._1))
    this

  def contains(k: Expr[K]): Boolean =
    exprs.exists(x => k.matches(x._1))

  def get(k: Expr[K]): Option[V] =
    exprs.find(x => k.matches(x._1)).map(_._2)
