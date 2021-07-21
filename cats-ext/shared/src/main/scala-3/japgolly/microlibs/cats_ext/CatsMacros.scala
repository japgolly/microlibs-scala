package japgolly.microlibs.cats_ext

import japgolly.microlibs.compiletime.MacroEnv.*
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import cats.Eq

object CatsMacros:

  inline def deriveEq[A]: Eq[A] =
    ${ deriveEqImpl[A](false) }

  inline def _deriveEq[A]: Eq[A] =
    ${ deriveEqImpl[A](true) }

  private def deriveEqImpl[A](debug: Boolean)(using Quotes, Type[A]): Expr[Eq[A]] =
    var result: Expr[Eq[A]] = null
    def log(msg: => Any) = if debug then println(msg)
    log("="*120)
    log(s"Beginning derivation of cats.Eq[${Type.show[A]}]")

    type Clause = MacroUtils.Fn2Clause[A, A, Boolean]

    def newInstance(f: Clause): Quotes ?=> Expr[Eq[A]] = '{
      Eq.instance[A]((x, y) => ${f('x, 'y)})
    }

    Expr.summon[Mirror.Of[A]] match

      case Some('{ $m: Mirror.ProductOf[A] }) =>
        result = MacroUtils.CachedGivens[Eq].mirror(m).summonGivens().use[Eq[A]] { ctx =>

          val clauses =
            ctx.fields.map[Clause](f => (x, y) => {
              val eq = ctx.lookup(f).expr
              '{ $eq.eqv(${f.onProduct(x)}, ${f.onProduct(y)}) }
            })

          MacroUtils.mergeFn2s(
            fs    = clauses,
            empty = Left(Expr(true)),
            merge = (x, y) => '{ $x && $y },
            outer = newInstance,
          )
        }

      case Some('{ $m: Mirror.SumOf[A] }) =>
        result = MacroUtils.CachedGivens[Eq].mirror(m).summonGivens().forSumType(m) { f =>
          newInstance { (x, y) => '{
              val o = ${f.ordinalOf(x)}
              (o == ${f.ordinalOf(y)}) && ${f.typeclassForOrd('o)}.eqv($x, $y)
            }
          }
        }

      case _ =>

    if result == null then
      val err = s"Don't know how to derive an Eq instance for ${Type.show[A]}: Mirror not found."
      log(err)
      log("="*120)
      quotes.reflect.report.throwError(err)
    else
      log(result.show)
      log("="*120)
      result
