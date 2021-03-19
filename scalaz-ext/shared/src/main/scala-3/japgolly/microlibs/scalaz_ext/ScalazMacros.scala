package japgolly.microlibs.scalaz_ext

import japgolly.microlibs.macro_utils.MacroUtils
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import scalaz.Equal
import MacroUtils.Ops._

object ScalazMacros:

  inline def deriveEqual[A]: Equal[A] =
    ${ deriveEqualImpl[A](false) }

  inline def _deriveEqual[A]: Equal[A] =
    ${ deriveEqualImpl[A](true) }

  private def deriveEqualImpl[A](debug: Boolean)(using Quotes, Type[A]): Expr[Equal[A]] =
    var result: Expr[Equal[A]] = null
    def log(msg: => Any) = if debug then println(msg)
    log("="*120)
    log(s"Beginning derivation of scalaz.Equal[${Type.show[A]}]")

    type Clause = MacroUtils.Fn2Clause[A, A, Boolean]

    def newInstance(f: Clause): Quotes ?=> Expr[Equal[A]] = '{
      new Equal[A] {
        override def equal(x: A, y: A) = ${f('x, 'y)}
      }
    }

    Expr.summon[Mirror.Of[A]] match

      // Product
      case Some('{ $m: Mirror.ProductOf[A] { type MirroredElemTypes = types } }) =>
        result = MacroUtils.withCachedGivens[A, Equal, Equal[A]](m) { lookup =>

          val clauses =
            MacroUtils.mirrorFields(m).map[Clause](f => (x, y) => {
              val eq = lookup(f)
              '{ $eq.equal(${f.onProduct(x)}, ${f.onProduct(y)}) }
            })

          MacroUtils.mergeFn2s(
            fs    = clauses,
            empty = Left(Expr(true)),
            merge = (x, y) => '{ $x && $y },
            outer = newInstance,
          )
        }

      // Sum
      case Some('{ $m: Mirror.SumOf[A] { type MirroredElemTypes = types } }) =>
        result = MacroUtils.buidTypeClassForSum[Equal, A](m) { b =>
          newInstance { (x, y) => '{
              val o = ${b.ordinal(x)}
              (o == ${b.ordinal(y)}) && ${b.tc('o)}.equal($x, $y)
            }
          }
        }

      case _ =>

    if result == null then
      val err = s"Don't know how to derive an Equal instance for ${Type.show[A]}: Mirror not found."
      log(err)
      log("="*120)
      quotes.reflect.report.throwError(err)
    else
      log(result.show)
      log("="*120)
      result
