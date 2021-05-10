package japgolly.microlibs.compiletime

import scala.quoted.*

object ExprSet:
  def empty[A](using Quotes): ExprSet[A] =
    new ExprSet[A]

final class ExprSet[A](using Quotes):
  private var exprs = List.empty[Expr[A]]

  def +=(e: Expr[A]): this.type =
    if !this.contains(e) then exprs ::= e
    this

  def -=(e: Expr[A]): this.type =
    exprs = exprs.filterNot(e.matches)
    this

  def contains(e: Expr[A]): Boolean =
    exprs.exists(e.matches)
