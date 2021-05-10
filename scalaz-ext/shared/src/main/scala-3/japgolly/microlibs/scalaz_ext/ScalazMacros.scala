package japgolly.microlibs.scalaz_ext

import japgolly.microlibs.macro_utils.MacroEnv.*
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import scalaz.Equal

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

      case Some('{ $m: Mirror.ProductOf[A] }) =>
        result = MacroUtils.CachedGivens[Equal].mirror(m).summonGivens().use[Equal[A]] { ctx =>

          val clauses =
            ctx.fields.map[Clause](f => (x, y) => {
              val eq = ctx.lookup(f).expr
              '{ $eq.equal(${f.onProduct(x)}, ${f.onProduct(y)}) }
            })

          MacroUtils.mergeFn2s(
            fs    = clauses,
            empty = Left(Expr(true)),
            merge = (x, y) => '{ $x && $y },
            outer = newInstance,
          )
        }

      case Some('{ $m: Mirror.SumOf[A] }) =>
        result = MacroUtils.CachedGivens[Equal].mirror(m).summonGivens().forSumType(m) { f =>
          newInstance { (x, y) => '{
              val o = ${f.ordinalOf(x)}
              (o == ${f.ordinalOf(y)}) && ${f.typeclassForOrd('o)}.equal($x, $y)
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
