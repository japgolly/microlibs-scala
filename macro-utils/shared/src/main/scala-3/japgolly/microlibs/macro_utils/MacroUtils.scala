package japgolly.microlibs.macro_utils

import scala.deriving.*
import scala.quoted.*
import scala.reflect.ClassTag

object MacroUtils:

  object Ops:
    // extension [A] (self: Expr[A])

    //   def debugPrint()(using Quotes): Expr[A] =
    //     println(s"\n${self.show}\n")
    //     self

    extension [F[_], A] (self: Expr[F[A]])

      inline def asFAny: Expr[F[Any]] =
        self.asInstanceOf[Expr[F[Any]]]

      def substFAny(using Quotes, Type[F], Type[A]): Expr[F[Any]] =
        '{ $self.asInstanceOf[F[Any]] }

    extension (using q: Quotes)(self: q.reflect.Term)
      def asConstant: q.reflect.Constant =
        import q.reflect.*
        self match {
          case Literal(c) => c
          case x => fail(s"Expected a constant literal, got: ${x.show} ")
        }

      def simplify: q.reflect.Term =
        import q.reflect.*
        self match
        case Block(_, t) => t
        case _           => self

  end Ops
  import Ops._

  inline def fail(msg: String)(using Quotes): Nothing =
    quotes.reflect.report.throwError(msg)

  def exprSingleton[A: Type](using Quotes): Option[Expr[A]] =
    Expr.summon[ValueOf[A]].map { e =>
      import quotes.reflect.*
      e.asTerm match
        case Apply(_, List(t)) => t.asExprOf[A]
        case _                 => '{ $e.value }
    }

  def exprSingletonOrThrow[A](using Type[A])(using Quotes): Expr[A] =
    exprSingleton[A].getOrElse(fail("Unable to get a singleton value for: " + Type.show[A]))

  def exprSummonOrThrow[A: Type](using Quotes): Expr[A] =
    Expr.summon[A] match
      case Some(e) => e
      case None    => fail(s"Could not find given ${Type.show[A]}")

  def exprSummonTupleFieldsOrThrow[A: Type](using Quotes): List[Expr[Any]] =
    Type.of[A] match
      case '[h *: t]     => exprSummonOrThrow[ToExpr[h]] :: exprSummonTupleFieldsOrThrow[t]
      case '[EmptyTuple] => Nil
      case _             => fail(s"${Type.show[A]} is not a fully-known tuple type")

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

  // def needMirrorSumOf[A: Type](using Quotes): Expr[Mirror.SumOf[A]] =
  //   Expr.summon[Mirror.Of[A]] match
  //     case Some('{ $m: Mirror.SumOf[A] }) => m
  //     case _ => fail(s"Not a sum type: ${Type.show[A]}")

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

  def mapByFieldTypes[A: Type, B](f: [C] => Type[C] ?=> B)(using q: Quotes): Map[q.reflect.TypeRepr, B] =
    import quotes.reflect.*

    var map = Map.empty[TypeRepr, B]

    def process[T: Type]: Unit =
      val t = TypeRepr.of[T]
      if !map.contains(t) then
        val b = f[T]
        map = map.updated(t, b)

    def go[T: Type]: Unit =
      Type.of[T] match
        case '[h *: t]     => process[h]; go[t]
        case '[EmptyTuple] =>
        case _             => process[T]

    go[A]
    map

  def setOfFieldTypes[A: Type](using q: Quotes): Set[q.reflect.TypeRepr] =
    import quotes.reflect.*

    var set = Set.empty[TypeRepr]

    def process[T: Type]: Unit =
      set += TypeRepr.of[T]

    def go[T: Type]: Unit =
      Type.of[T] match
        case '[h *: t]     => process[h]; go[t]
        case '[EmptyTuple] =>
        case _             => process[T]

    go[A]
    set

  type FieldLookup[F[_]] = (f: Field) => Expr[F[f.Type]]

  def withCachedGivens[A: Type, F[_]: Type, B: Type](m: Expr[Mirror.Of[A]])
                                                    (use: FieldLookup[F] => Expr[B])
                                                    (using Quotes): Expr[B] =
    import quotes.reflect.*

    def result[T: Type]: Expr[B] =
      val summonMap = mapByFieldTypes[T, Expr[F[Any]]]([t] => (t: Type[t]) ?=> exprSummonOrThrow[F[t]].asFAny)
      val summons = summonMap.toArray
      val terms = summons.iterator.map(_._2.asTerm).toList
      ValDef.let(Symbol.spliceOwner, terms) { refs =>
        val lookupFn: FieldLookup[F] =
          f => {
            def fieldType = TypeRepr.of(using f.fieldType)
            val i = summons.indexWhere(_._1 == fieldType)
            if i < 0 then
              val t = Type.show[F[f.Type]]
              fail(s"Failed to find given $t in cache")
            refs(i).asExprOf[F[f.Type]]
          }
        use(lookupFn).asTerm
      }.asExprOf[B]

    Expr.summon[Mirror.Of[A]] match
      case Some('{ $m: Mirror.ProductOf[A] { type MirroredElemTypes = types } }) =>
        result[types]
      case Some('{ $m: Mirror.SumOf[A] { type MirroredElemTypes = types } }) =>
        result[types]
      case _ =>
        fail(s"Mirror not found for ${Type.show[A]}")

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

  def withNonEmptySumTypeTypes[A, B](a: Type[A])
                                    // (f: [t] => Type[t] ?=> Expr[Mirror.SumOf[A] { type MirroredElemTypes = t }] => B)
                                    (f: [t] => Type[t] ?=> B)
                                    (using Quotes): B =
    given Type[A] = a
    Expr.summon[Mirror.Of[A]] match

      case Some('{ $m: Mirror.SumOf[A] { type MirroredElemTypes = EmptyTuple }}) =>
        fail(s"${Type.show[A]} has no concrete cases.")

      case Some('{ $m: Mirror.SumOf[A] { type MirroredElemTypes = types }}) =>
        f[types]

      case _ =>
        fail(s"Not a sum type: ${Type.show[A]}")

  def logAll[A](name: String, as: Iterable[A])(f: A => Any): Unit =
    val xs = as.toIndexedSeq
    println(s"$name (${xs.length}):")
    for (i <- xs.indices)
      val a = xs(i)
      println(s"  $i) ${f(a)}")

  def extractCaseDefs[T, V](e: Expr[T => V])(using q: Quotes): List[q.reflect.CaseDef] =
    import quotes.reflect.*
    def go(tree: Tree): List[CaseDef] =
      tree match
        case Inlined(_, _, t) =>
          go(t)
        case Block(List(DefDef(_, _, _, Some(body))), _) =>
          go(body)
        case Match(_, cases) =>
          cases
        case x =>
          fail(s"Don't know how to extract cases from:\n  ${e.show}\nStuck on tree:\n  $tree")
    go(e.asTerm)

  // Ref      = `case Object`
  // TypeTree = `case _: Class`
  def extractInlineAdtMappingFn[T, V](e: Expr[T => V])(using q: Quotes)
      : List[(Either[q.reflect.Ref, q.reflect.TypeTree], q.reflect.Term)] =
    import quotes.reflect.*
    // logAll("CaseDefs", extractCaseDefs(e))(identity)
    extractCaseDefs(e).map {

      // case Object => "k"
      case CaseDef(r: Ref, _, body) =>
        (Left(r), body.simplify)

      // case _: Class => "k"
      case CaseDef(Typed(_, tt: TypeTree), _, body) =>
        (Right(tt), body.simplify)

      case x =>
        fail(s"Expecting a case like: {case _: Type => ?}\n  Got: $x\n  In: ${e.show}")
    }

  def anonymousMatch[A: Type, B: Type](using q: Quotes)(cases: Seq[q.reflect.CaseDef]): Expr[A => B] =
    import quotes.reflect.*
    def matchOn(a: Expr[A]): Expr[B] =
      Match(a.asTerm, cases.toList).asExprOf[B]
    '{ (a: A) => ${matchOn('a)} }

  def showUnorderedTypes(using q: Quotes)(ts: Set[q.reflect.TypeRepr]): String =
    ts.toList.map(_.toString).sorted.mkString(", ")
