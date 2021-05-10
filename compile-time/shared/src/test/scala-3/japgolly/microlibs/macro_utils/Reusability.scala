package japgolly.microlibs.compiletime

import scala.deriving.*
import scala.quoted.*
import MacroEnv.*

final class Reusability[A](val test: (A, A) => Boolean) {
  def contramap[B](f: B => A): Reusability[B] =
    new Reusability((x, y) => test(f(x), f(y)))
}

object Reusability {
  implicit final class ReactExtrasExt_Any[A](private val self: A) extends AnyVal {
    @inline def ~=~(a: A)(implicit r: Reusability[A]): Boolean = r.test(self, a)
    @inline def ~/~(a: A)(implicit r: Reusability[A]): Boolean = !r.test(self, a)
  }
  def always[A]: Reusability[A] = new Reusability[A]((_, _) => true)
  def by[A, B](f: A => B)(implicit r: Reusability[B]): Reusability[A] = r contramap f
  def suspend[A](f: => Reusability[A]): Reusability[A] = new Reusability((a, b) => f.test(a, b))
  def by_==[A]: Reusability[A] = new Reusability((a, b) => a == b)
  @inline implicit def int: Reusability[Int] = by_==
  implicit def string: Reusability[String] = by_==
  def indexedSeq[S[X] <: IndexedSeq[X], A: Reusability]: Reusability[S[A]] = Reusability((x, y) => (x.length == y.length) && x.indices.forall(i => x(i) ~=~ y(i)))
  implicit def vector[A: Reusability]: Reusability[Vector[A]] = indexedSeq[Vector, A]
  implicit def optionLike[A](implicit r: Reusability[A]): Reusability[Option[A]] = Reusability((x, y) => x.fold(y.isEmpty)(xa => y.fold(false)(ya => xa ~=~ ya)))

  transparent inline def derive[A]: Reusability[A] = ${
    deriveImpl[A](
      logNonReuse = false,
      logCode     = false,
    )
  }

  transparent inline def deriveDebug[A]: Reusability[A] = ${
    deriveImpl[A](
      logNonReuse = false,
      logCode     = true,
    )
  }

  private def nonReuseHeader[A: Type](using Quotes): Expr[String] =
    Expr.inlineConst(s"Instance of ${Type.show[A]} not-reusable for the following reasons:\n")

  private def nonReuseDesc[A: Type](desc: String, a: Expr[A], b: Expr[A])(using Quotes): Expr[String] = {
    val msg1 = Expr.inlineConst(desc + "\n  A: ")
    val msg2 = Expr.inlineConst("\n  B: ")
    '{ "  " + ($msg1 + $a + $msg2 + $b).replace("\n", "\n  ") }
  }

  private class Preparations(logCode: Boolean)(using q: Quotes) {
    import q.reflect.*

    private val init1  = Init("a" + _)
    private val init2  = Init("b" + _, Flags.Lazy)
    private val lazies = List.newBuilder[Statement]
    private var preps  = Map.empty[TypeRepr, Prepared[_]]
    private val stable = collection.mutable.HashMap.empty[TypeRepr, Expr[Reusability[Any]]]

    private def normalise[A](using t: Type[A]): TypeRepr =
      TypeRepr.of[A].dealias

    def addNow[A: Type](expr: Expr[Reusability[A]]): Prepared[A] = {
      val vd = init1.valDef(expr, extraFlags = Flags.Implicit)
      val p = Prepared[A](Type.of[A], vd.ref, None)
      preps = preps.updated(normalise[A], p)
      p
    }

    def addDeferred[A: Type](complete: => Expr[Reusability[A]]): Prepared[A] = {
      import Flags.*
      val name = init1.newName()
      val theVar = init1.valDef('{new Reusability[A](null)}, name = name, extraFlags = Mutable)
      val theVal = init1.valDef('{Reusability.suspend(${theVar.ref})}, name = s"_$name", extraFlags = Implicit | Lazy, onInit = false)
      lazies += theVal.valDef
      lazy val assignComplete = theVar.assign(complete)
      val p = Prepared[A](Type.of[A], theVar.ref, Some(() => assignComplete))
      preps = preps.updated(normalise[A], p)
      p
    }

    /** @param typ Just `A`, not `Reusable[A]` */
    def get[A](using t: Type[A]): Option[Prepared[A]] =
      val t = normalise[A]
      preps.get(t).map(_.subst[A])

    /** @param typ Just `A`, not `Reusable[A]` */
    def need[A](using t: Type[A]): Prepared[A] =
      get[A].getOrElse(throw new IllegalStateException(s"Prepared type for ${normalise[A].show} not found!"))

    def getOrSummonLater[A](using t: Type[A]): Expr[Reusability[A]] =
      get[A] match
        case Some(p) => p.varRef
        case None    => Expr.summonLater[Reusability[A]]

    def getStablisedImplicitInstance[A: Type]: Expr[Reusability[A]] =
      def target: Expr[Reusability[A]] =
        get[A] match
          case Some(p) => p.varRef
          case None    => init2.valDef(Expr.summonLater[Reusability[A]]).ref
      stable
        .getOrElseUpdate(TypeRepr.of[A], target.asExprOfFAny)
        .asExprOfF[A]

    def stabliseInstance[A: Type](e: Expr[Reusability[A]]): Expr[Reusability[A]] =
      init2.valDef(e).ref

    def result[A: Type](finalResult: Prepared[A]): Expr[Reusability[A]] = {
      init1 ++= lazies.result()
      val result: Expr[Reusability[A]] =
        init1.wrapExpr {
          init2.wrapExpr {
            val allPreps = preps.valuesIterator.flatMap(_.complete.map(_())).toList
            Expr.block(allPreps, finalResult.varRef)
          }
        }

      if (logCode)
        // TODO: println(s"\nDerived ${result.showType}:\n${result.show}\n")
        println(s"\nDerived ${result.showType}:\n${result.show.replace("japgolly.microlibs.macro_utils.","").replace(".apply(","(").replace("scala.", "").replace("RecursiveDerivationTest.", "")}\n")

      result
    }
  }

  private object Preparations:
    def apply[A: Type](logCode: Boolean)(f: Preparations => Prepared[A])(using Quotes): Expr[Reusability[A]] =
      val preparations = new Preparations(logCode)
      val prepared     = f(preparations)
      preparations.result(prepared)

  /** @param typ Just `A`, not `Reusable[A]` */
  private case class Prepared[A](typ: Type[A], varRef: Expr[Reusability[A]], complete: Option[() => Expr[Unit]]):
    def subst[B] = this.asInstanceOf[Prepared[B]]

  private def deriveImpl[A: Type](logNonReuse: Boolean,
                                  logCode    : Boolean)(using Quotes): Expr[Reusability[A]] =
    Preparations(logCode = logCode) { preparations =>
      deriveImplInner[A](
        logNonReuse  = logNonReuse,
        preparations = preparations,
      )
    }

  private def caseClassImpl[A: Type](mirror: Expr[Mirror.ProductOf[A]],
                                     logNonReuse: Boolean,
                                     logCode    : Boolean,
                                    //  exclusions : Seq[c.Expr[String]]
                                     )(using q: Quotes): Expr[Reusability[A]] =
    Preparations(logCode = logCode) { preparations =>
      caseClassImplInner[A](
        logNonReuse  = logNonReuse,
        fields       = Fields.fromMirror(mirror),
        preparations = preparations,
        // exclusions   = exclusions,
      )
    }

  // ===================================================================================================================

  private def deriveImplInner[A: Type](logNonReuse: Boolean, preparations: Preparations)(using Quotes): Prepared[A] =
    Expr.summonOrError[Mirror.Of[A]] match

      case '{ $m: Mirror.ProductOf[A] } =>
        caseClassImplInner[A](
          logNonReuse  = logNonReuse,
          fields       = Fields.fromMirror(m),
          preparations = preparations,
          // exclusions   = Nil,
        )

      // case '{ type ts <: Tuple; $m: Mirror.SumOf[A] { type MirroredElemTypes = `ts` }} =>
      case '{ $m: Mirror.SumOf[A] } =>
        sumTypeImplInner[A](
          mirror       = m,
          derive       = true,
          logNonReuse  = logNonReuse,
          preparations = preparations,
        )

  private def sumTypeImplInner[A: Type](
      mirror      : Expr[Mirror.SumOf[A]],
      derive      : Boolean,
      logNonReuse : Boolean,
      preparations: Preparations)(using Quotes): Prepared[A] = {

    val fields = Fields.fromMirror(mirror)
    val fieldCount = fields.size

    def mkTest[B: Type](e: Expr[Reusability[B]], stablise: Boolean): Expr[Test] =
      val instance = if stablise
        then preparations.stabliseInstance(Expr.summonLater[Reusability[B]])
        else e
      '{ (a: A, b: A) => $instance.test(a.asInstanceOf[B], b.asInstanceOf[B]) }

    type Test = (A, A) => Boolean

    val nonRecursiveCases = Array.fill[Option[Expr[Test]]](fieldCount)(None)
    for (f <- fields) {
      import f.{Type => F, typeInstance}
      Expr.summon[Reusability[F]] match
        case Some(rf) =>
          val test = mkTest[F](rf, stablise = true)
          nonRecursiveCases(f.idx) = Some(test)
        case None =>
          if derive then
            deriveImplInner[F](logNonReuse = logNonReuse, preparations = preparations)
          else
            Expr.summonOrError[Reusability[F]]
            ???
    }

    preparations.addDeferred[A] {

      val tests = Array.fill[Expr[Test]](fieldCount)(null)
      for (f <- fields) {
        import f.{Type => F, idx => i, typeInstance}
        val test: Expr[Test] =
          nonRecursiveCases(i).getOrElse {
            if derive then
              val p = preparations.need[F]
              mkTest[F](p.varRef, stablise = false)
            else
              Expr.summonOrError[Reusability[F]]
              ???
          }
        tests(i) = test
      }

      val testArray = MacroUtils.mkArrayExpr(tests.toIndexedSeq)

      '{
        val m = $mirror
        val tests = $testArray
        Reusability[A]((a, b) =>
          val o = m.ordinal(a)
          (o == m.ordinal(b)) && tests(o)(a, b)
        )

      }
    }
  }

  private def caseClassImplInner[A: Type](logNonReuse : Boolean,
    fields: List[Field],
                                                   preparations: Preparations,
                                                  //  exclusions  : Seq[c.Expr[String]]
                                                   )(using Quotes): Prepared[A] =

    fields match
      case Nil =>
        preparations.addNow[A] {
          '{ Reusability.always[A] }
        }

      case f :: Nil if !logNonReuse =>
        import f.{Type => F, typeInstance}
        preparations.addDeferred[A] {
          val imp = preparations.getOrSummonLater[F]
          '{ Reusability.by[A, F](a => ${f.onProduct('a)})($imp) }
        }

      case _ =>
        preparations.addDeferred[A] {
          import quotes.reflect.*

          var tests                = Vector.empty[Expr[(A, A) => Boolean]]
          var testsLoggingNonReuse = Vector.empty[Expr[(A, A) => Unit]]
          val failures             = typedValDef[List[String]]("failures", Flags.Mutable)('{Nil})

          for (f <- fields) {
            import f.{Type => F, typeInstance}
            val fp = preparations.getStablisedImplicitInstance[F]

            val test = '{ (a: A, b: A) => $fp.test(${f.onProduct('a)}, ${f.onProduct('b)}) }
            tests :+= test

            if logNonReuse then
              testsLoggingNonReuse :+= '{ (a: A, b: A) =>
                if !${test('a, 'b)} then
                  ${failures.modify(fs => '{ $fs :+ ${nonReuseDesc(s".${f.name} values not reusable", 'a, 'b)}})}
              }
          }

          if logNonReuse then
            val header = nonReuseHeader[A]
            '{
              Reusability[A]((a, b) =>
                ${ failures.use(f => '{
                  ${ testsLoggingNonReuse.iterator.map(_('a, 'b)).reduce((x, y) => '{$x; $y}) }
                  if $f.nonEmpty then
                    println($f.sorted.mkString($header, "\n", ""))
                  $f.isEmpty
                })}
              )
            }
          else
            '{
              Reusability[A]((a, b) =>
                ${ tests.iterator.map(_('a, 'b)).reduce((x, y) => '{$x && $y}) }
              )
            }
        }

}