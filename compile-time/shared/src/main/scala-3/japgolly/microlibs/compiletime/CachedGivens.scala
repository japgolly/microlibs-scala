package japgolly.microlibs.compiletime

import scala.collection.immutable.ArraySeq
import scala.deriving.*
import scala.quoted.*
import MacroEnv.*

object CachedGivens:

  final case class Value[F[_], A](expr: Expr[F[A]]):
    def subst[B] = this.asInstanceOf[Value[F, B]]

  type FieldLookup[F[_]] = (f: Field) => Value[F, f.Type]

  type Materialiser[F[_]] = Env[F] => (f: Field) => Option[Expr[F[f.Type]]]

  object Materialiser:
    def none[F[_]]: Materialiser[F] =
      _ => (f: Field) => None

  final case class SumTypeClassBuilder[-A, +TC](ordinalOf      : Expr[A] => Expr[Int],
                                                typeclassForOrd: Expr[Int] => Expr[TC]):
    def typeclassFor(value: Expr[A]): Expr[TC] =
      typeclassForOrd(ordinalOf(value))

  final case class Ctx[F[_]](fields: ArraySeq[Field], lookup: FieldLookup[F])(using q: Quotes)(using Type[F]):
    import q.reflect.*

    lazy val givensArray: Expr[Array[F[Any]]] =
      MacroUtils.mkArrayExprF(fields.map(lookup(_).expr.castToFAny))

    def forSumType[A](m: Expr[Mirror.SumOf[A]])
                     (f: SumTypeClassBuilder[A, F[Any]] => Expr[F[A]])
                     (using Type[A]): Expr[F[A]] =
      val givens = givensArray
      ValDef.let(Symbol.spliceOwner, "m", m.asTerm) { _m =>
        val m = _m.asExprOf[Mirror.SumOf[A]]
        ValDef.let(Symbol.spliceOwner, "g", givens.asTerm) { _givens =>
          val givens = _givens.asExprOf[Array[F[Any]]]

          val builder = SumTypeClassBuilder[A, F[Any]](
            ordinalOf       = a => '{$m.ordinal($a)},
            typeclassForOrd = o => '{$givens($o)},
          )

          f(builder).asTerm
        }
      }.asExprOf[F[A]]

  end Ctx

  // ===================================================================================================================

  object Env {
    def apply[F[_]: Type](qq: Quotes): Env[F] { val q: qq.type } =
      new Env(using qq).asInstanceOf[Env[F] { val q: qq.type }] // TODO: raise Scala bug
  }

  final class Env[F[_]](using val q: Quotes)(using Type[F]) { self =>
    import quotes.reflect.*

    private var _env = Map.empty[TypeRepr, Value[F, Any]]

    private def showFA(t: TypeRepr): String =
      s"${Type.show[F]}[${t.show}]"

    private def showFA[A](using t: Type[A]): String =
      showFA(TypeRepr.of(using t))

    abstract class ByTypeDsl {
      type Out[A]

      def byTypeRepr(t: TypeRepr): Out[Any] =
        t.asType match {
          case '[a] => byType[a].asInstanceOf[Out[Any]]
        }

      def byTypeTree(t: TypeTree): Out[Any] =
        byTypeRepr(t.tpe)

      def byType[A](using t: Type[A]): Out[A] =
        byTypeRepr(TypeRepr.of(using t)).asInstanceOf[Out[A]]

      def byField(f: Field): Out[f.Type] =
        byType[f.Type](using f.typeInstance)
    }

    object contains extends ByTypeDsl:
      override type Out[A] = Boolean
      override def byTypeRepr(t: TypeRepr) = _env.contains(t)

    object add:
      def byTypeRepr[A](t: TypeRepr, e: Expr[F[A]]): Unit =
        _env.get(t) match
          case None     => _env = _env.updated(t, Value(e).subst[Any])
          case Some(e2) => if !e.matches(e2.expr) then fail(s"Conflicting instances of ${showFA(t)} added.")

      def byTypeTree[A](t: TypeTree, e: Expr[F[A]]): Unit =
        byTypeRepr(t.tpe, e)

      def byType[A](e: Expr[F[A]])(using t: Type[A]): Unit =
        byTypeRepr(TypeRepr.of(using t), e)

      def byField(f: Field, e: Expr[F[f.Type]]): Unit =
        byType[f.Type](e)(using f.typeInstance)

    object summon extends ByTypeDsl:
      override type Out[A] = Unit
      override def byType[A](using t: Type[A]): Unit =
        Expr.summon[F[A]] match
          case Some(e) => add.byType(e)
          case None    => fail(s"Failed to find an instance of ${showFA[A]}.")

    def summonOrMaterialise(f: Field, m: Materialiser[F]): Unit =
      type A = f.Type
      import f.typeInstance
      Expr.summon[F[A]] match
        case Some(e) => add.byType[A](e)
        case None    => m(self)(f) match
          case Some(e) => add.byType[A](e)
          case None    => fail(s"Failed to find or create an instance of ${showFA[A]}.")

    object get extends ByTypeDsl:
      override type Out[A] = Option[Value[F, A]]
      override def byTypeRepr(t: TypeRepr) = _env.get(t)

    object need extends ByTypeDsl:
      override type Out[A] = Value[F, A]
      override def byTypeRepr(t: TypeRepr) =
        get.byTypeRepr(t) getOrElse fail(s"Failed to find an instance of ${showFA(t)}.")

    def use[A](fields: ArraySeq[Field])(use: Ctx[F] => Expr[A])(using Type[A], Type[F]): Expr[A] =
      val summons: Array[(TypeRepr, Value[F, Any])] =
        _env.toArray

      val terms: List[Term] =
        summons.iterator.map(_._2.expr.asTerm).toList

      ValDef.let(Symbol.spliceOwner, terms) { refs =>
        val lookupFn: FieldLookup[F] =
          f => {
            val fieldType = f.typeRepr
            val i = summons.indexWhere(_._1 == fieldType)
            if i < 0 then fail(s"Failed to find given F[${f.showType}] in cache")
            val e = refs(i).asExprOf[F[f.Type]]
            Value(e)
          }
        val ctx = Ctx(fields, lookupFn)
        use(ctx).asTerm
      }.asExprOf[A]
    end use

  }

  // ===================================================================================================================

  def apply[F[_]] = new Dsl1[F]

  final class Dsl1[F[_]]:

    def summonMirrorOrError[A](using Quotes, Type[A]): Dsl2[F] =
      Expr.summon[Mirror.Of[A]] match {
        case Some(m) => mirror(m)
        case None    => fail(s"Mirror not found for ${Type.show[A]}")
      }

    def mirror[A](m: Expr[Mirror.Of[A]])(using Quotes, Type[A]): Dsl2[F] =
      fields(Fields.fromMirror(m))

    def fields(fs: IterableOnce[Field]): Dsl2[F] =
      new Dsl2(fs.iterator.to(ArraySeq), Materialiser.none)

  final class Dsl2[F[_]](fields: ArraySeq[Field], materialiser: Materialiser[F]):

    def filter(f: Field => Boolean): Dsl2[F] =
      new Dsl2(fields filter f, materialiser)

    def filterWhen(cond: Boolean)(f: => (Field => Boolean)): Dsl2[F] =
      if cond then filter(f) else this

    def materialise(m: Materialiser[F]): Dsl2[F] =
      new Dsl2(fields, c => {
        val mm1 = materialiser(c)
        val mm2 = m(c)
        (f: Field) => mm1(f).orElse(mm2(f))
      })

    def summonGivens()(using q: Quotes)(using Type[F]): Dsl3[F] =
      val col = Env[F](q)
      for (f <- fields)
        col.summonOrMaterialise(f, materialiser)
      new Dsl3(fields, col)(using q)

  final class Dsl3[F[_]](fields: ArraySeq[Field], col: Env[F])(using q: Quotes):

    def use[A](use: Ctx[F] => Expr[A])(using Type[A], Type[F]): Expr[A] =
      col.use(fields)(use)

    def forSumType[A](m: Expr[Mirror.SumOf[A]])
                     (f: SumTypeClassBuilder[A, F[Any]] => Expr[F[A]])
                     (using Type[A], Type[F]): Expr[F[A]] =
    use(_.forSumType(m)(f))
