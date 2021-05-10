package japgolly.microlibs.compiletime

import scala.deriving.*
import scala.quoted.*
import scala.reflect.ClassTag

object MacroUtils:
  import MacroEnv.*

  export japgolly.microlibs.compiletime.{
    CachedGivens,
    NewInstance,
  }

  def logAll[A](name: String, as: Iterable[A])(f: A => Any): Unit =
    val xs = as.toIndexedSeq
    println(s"$name (${xs.length}):")
    for (i <- xs.indices)
      val a = xs(i)
      println(s"  [${i+1}/${xs.length}] ${f(a)}")

  def getSingletonValueForType[A: Type](using Quotes): Option[Expr[A]] =
    Expr.summon[ValueOf[A]].map { e =>
      import quotes.reflect.*
      e.asTerm match
        case Apply(_, List(t)) => t.asExprOf[A]
        case _                 => '{ $e.value }
    }

  def needSingletonValueForType[A](using Type[A])(using Quotes): Expr[A] =
    getSingletonValueForType[A].getOrElse(fail("Unable to get a singleton value for: " + Type.show[A]))

  def needGiven[A: Type](using Quotes): Expr[A] =
    Expr.summon[A] match
      case Some(e) => e
      case None    => fail(s"Could not find given ${Type.show[A]}")

  def needGivensInTuple[A: Type](using Quotes): List[Expr[Any]] =
    Type.of[A] match
      case '[h *: t]     => needGiven[ToExpr[h]] :: needGivensInTuple[t]
      case '[EmptyTuple] => Nil
      case _             => fail(s"${Type.show[A]} is not a fully-known tuple type")

  def mkArrayExpr[A: Type](as: Seq[Expr[A]])(using Quotes): Expr[Array[A]] =
    val ct = needGiven[ClassTag[A]]
    '{ Array(${Varargs(as)}: _*)(using $ct) }

  def mkArrayExprF[F[_]: Type, A](as: Seq[Expr[F[A]]])(using Quotes): Expr[Array[F[Any]]] =
    mkArrayExpr[F[Any]](as.map(_.asExprOfFAny))

  def mkVectorExpr[A: Type](as: Seq[Expr[A]])(using Quotes): Expr[Vector[A]] =
    '{ Vector(${Varargs(as)}: _*) }

  def mkVectorExprF[F[_]: Type, A](as: Seq[Expr[F[A]]])(using Quotes): Expr[Vector[F[Any]]] =
    mkVectorExpr[F[Any]](as.map(_.asExprOfFAny))

  // def needMirrorSumOf[A: Type](using Quotes): Expr[Mirror.SumOf[A]] =
  //   Expr.summon[Mirror.Of[A]] match
  //     case Some('{ $m: Mirror.SumOf[A] }) => m
  //     case _ => fail(s"Not a sum type: ${Type.show[A]}")

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

  def reduceSeq[A, B](as: Seq[A], empty: => B, one: A => B, many: Seq[A] => B): B =
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
    reduceSeq[Fn2Clause[A, B, X], Expr[Y]](
      as    = fs,
      empty = empty.fold(x => outer((_, _) => x), identity),
      one   = outer,
      many  = fs => outer((x, y) => fs.iterator.map(_(x, y)).reduce(merge)),
    )

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

  def mkAnonymousMatch[A: Type, B: Type](using q: Quotes)(cases: Seq[q.reflect.CaseDef]): Expr[A => B] =
    import quotes.reflect.*
    def matchOn(a: Expr[A]): Expr[B] =
      Match(a.asTerm, cases.toList).asExprOf[B]
    '{ (a: A) => ${matchOn('a)} }

  def showUnorderedTypes(using q: Quotes)(ts: Set[q.reflect.TypeRepr]): String =
    ts.iterator.map(_.toString).toArray.sorted.mkString(", ")
