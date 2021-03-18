package japgolly.microlibs.macro_utils

import scala.deriving.*
import scala.quoted.*
import scala.reflect.ClassTag

object MacroUtils {

  object Ops {
    extension [F[_], A] (self: Expr[F[A]])

      inline def asFAny: Expr[F[Any]] =
        self.asInstanceOf[Expr[F[Any]]]

      def substFAny(using Quotes, Type[F], Type[A]): Expr[F[Any]] =
        '{ $self.asInstanceOf[F[Any]] }
  }
  import Ops._

  def exprSummonOrThrow[A: Type](using Quotes): Expr[A] =
    Expr.summon[A] match
      case Some(e) => e
      case None    => quotes.reflect.report.throwError(s"Could not find given ${Type.show[A]}")

  def exprSummonTupleFieldsOrThrow[A: Type](using Quotes): List[Expr[Any]] =
    Type.of[A] match
      case '[h *: t]     => exprSummonOrThrow[ToExpr[h]] :: exprSummonTupleFieldsOrThrow[t]
      case '[EmptyTuple] => Nil
      case _             => quotes.reflect.report.throwError(s"${Type.show[A]} is not a fully-known tuple type")

  def exprArrayOf[A: Type](as: Seq[Expr[A]])(using Quotes): Expr[Array[A]] =
    val ct = exprSummonOrThrow[ClassTag[A]]
    '{ Array(${Varargs(as)}: _*)(using $ct) }

  def exprArrayOfF[F[_]: Type, A](as: Seq[Expr[F[A]]])(using Quotes): Expr[Array[F[Any]]] =
    exprArrayOf[F[Any]](as.map(_.asFAny))

  trait Field:
    val idx: Int
    type Name
    type Type

    implicit val fieldType: scala.quoted.Type[Type]

    final def onProduct[P](p: Expr[P])(using Quotes, scala.quoted.Type[P]): Expr[Type] =
      '{ $p.asInstanceOf[Product].productElement(${Expr(idx)}).asInstanceOf[Type] }

  end Field

  def mirrorFields[A: Type, B](m: Expr[Mirror.Of[A]])(using Quotes): List[Field] =
    import quotes.reflect.*

    def go[Ls: Type, Ts: Type](idx: Int): List[Field] =
      (Type.of[Ls], Type.of[Ts]) match
        case ('[l *: ll], '[t *: tt]) =>
          val t = Type.of[t]
          val L = TypeRepr.of[l]
          val f: Field = new Field {
            override val idx                = idx
            override type Name              = l
            override type Type              = t
            override implicit val fieldType = t
          }
          f :: go[ll, tt](idx + 1)

        case ('[EmptyTuple], _) =>
          Nil

    m match
      case '{ $m: Mirror.ProductOf[A] { type MirroredElemLabels = ls; type MirroredElemTypes = ts }} =>
        go[ls, ts](0)
      case '{ $m: Mirror.SumOf[A] { type MirroredElemLabels = ls; type MirroredElemTypes = ts }} =>
        go[ls, ts](0)

  type FieldLookup[F[_]] = (f: Field) => Expr[F[f.Type]]

  def withCachedGivens[A: Type, F[_]: Type, B: Type](m: Expr[Mirror.Of[A]])
                                                    (use: FieldLookup[F] => Expr[B])
                                                    (using Quotes): Expr[B] =
    import quotes.reflect.*

    val summonMap = collection.mutable.Map.empty[TypeRepr, Expr[F[Any]]]

    def prepare[T: Type]: Unit =
      Type.of[T] match
        case '[h *: t] =>
          val h = TypeRepr.of[h]
          if !summonMap.contains(h) then
            val s = exprSummonOrThrow[F[h]]
            summonMap.update(h, s.asFAny)
          prepare[t]
        case '[EmptyTuple] =>

    def result(): Expr[B] =
      val summons = summonMap.toArray
      val terms = summons.iterator.map(_._2.asTerm).toList
      ValDef.let(Symbol.spliceOwner, terms) { refs =>
        val lookupFn: FieldLookup[F] =
          f => {
            def fieldType = TypeRepr.of(using f.fieldType)
            val i = summons.indexWhere(_._1 == fieldType)
            if i < 0 then
              val t = Type.show[F[f.Type]]
              quotes.reflect.report.throwError(s"Failed to find given $t in cache")
            refs(i).asExprOf[F[f.Type]]
          }
        use(lookupFn).asTerm
      }.asExprOf[B]

    Expr.summon[Mirror.Of[A]] match
      case Some('{ $m: Mirror.ProductOf[A] { type MirroredElemTypes = types } }) =>
        prepare[types]
        result()
      case Some('{ $m: Mirror.SumOf[A] { type MirroredElemTypes = types } }) =>
        prepare[types]
        result()
      case _ =>
        quotes.reflect.report.throwError(s"Mirror not found for ${Type.show[A]}")

  def seqMerge[A, B](as: Seq[A], empty: => B, one: A => B, many: Seq[A] => B): B =
    if (as.isEmpty)
      empty
    else if (as.sizeIs == 1)
      one(as.head)
    else
      many(as)

  type Fn2Clause[A, B, X] = Quotes ?=> (Expr[A], Expr[B]) => Expr[X]

  def mergeFn2s[A, B, X, Y](fs   : Seq[Fn2Clause[A, B, X]],
                            empty: => Either[Expr[X], Expr[Y]],
                            outer: Fn2Clause[A, B, X] => Expr[Y],
                            merge: Fn2Clause[X, X, X]
                           ): Expr[Y] =
    seqMerge[Fn2Clause[A, B, X], Expr[Y]](
      as    = fs,
      empty = empty.fold(x => outer((_, _) => x), identity),
      one   = outer,
      many  = fs => outer((x, y) => fs.iterator.map(_(x, y)).reduce(merge)),
    )

  final case class TypeClassForSumBuilder[-A, +F](ordinal: Expr[A] => Expr[Int],
                                                  tc: Expr[Int] => Expr[F])

  def buidTypeClassForSum[F[_]: Type, A: Type](m: Expr[Mirror.SumOf[A]])
                                              (f: TypeClassForSumBuilder[A, F[Any]] => Expr[F[A]])
                                              (using Quotes): Expr[F[A]] =
    import quotes.reflect.*
    withCachedGivens[A, F, F[A]](m) { lookup =>

      val fields = mirrorFields(m)
      // val givens = exprArrayOfF(fields.map(lookup(_).substFAny))
      // TODO Delete ↓ and restore ↑ after Scala 3.0.0-RC2
      val givens = exprArrayOfF(fields.map {f => '{ ${lookup(f)}.asInstanceOf[F[Any]] }})

      ValDef.let(Symbol.spliceOwner, "m", m.asTerm) { _m =>
        val m = _m.asExprOf[Mirror.SumOf[A]]
        ValDef.let(Symbol.spliceOwner, "g", givens.asTerm) { _givens =>
          val givens = _givens.asExprOf[Array[F[Any]]]

          val builder = TypeClassForSumBuilder[A, F[Any]](
            ordinal = a => '{$m.ordinal($a)},
            tc      = o => '{$givens($o)},
          )

          f(builder).asTerm
        }
      }.asExprOf[F[A]]
    }
}
