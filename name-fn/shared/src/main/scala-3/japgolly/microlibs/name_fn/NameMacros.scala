package japgolly.microlibs.name_fn

import scala.compiletime.*
import scala.quoted.*

trait NameImplicits {

  inline implicit def materializeNameFromString(inline body: String): Name =
    ${ NameMacros.name('body) }

  inline implicit def materializeNameFnFromString(inline body: String): NameFn[Any] =
    NameFn.const(body)

  inline implicit def nameFnFromString[A](a: A)(using ev: A => Name): NameFn[Any] =
    NameFn const ev(a)
}

object NameMacros {
  def name(expr: Expr[String])(using Quotes): Expr[Name] =
    if expr.value.isDefined then
      '{ Name.now($expr) }
    else
      '{ Name($expr) }
}
