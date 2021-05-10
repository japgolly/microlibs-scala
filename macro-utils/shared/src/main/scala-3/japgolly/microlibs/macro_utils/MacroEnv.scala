package japgolly.microlibs.macro_utils

import scala.annotation.targetName
import scala.quoted.*

object MacroEnv {

  export japgolly.microlibs.macro_utils.{
    Field,
    Fields,
  }

  def fail(msg: String)(using Quotes): Nothing =
    import quotes.reflect.*
    quotes.reflect.report.throwError(msg, Position.ofMacroExpansion)

  def failNoStack(msg: String): Nothing = {
    val e = new RuntimeException(msg)
    e.setStackTrace(Array.empty)
    throw e
  }

  // ===================================================================================================================
  extension (unused: Expr.type) {

    def summonOrError[A](using Type[A])(using Quotes): Expr[A] =
      import quotes.reflect.*
      Implicits.search(TypeRepr.of[A]) match
        case iss: ImplicitSearchSuccess => iss.tree.asExpr.asInstanceOf[Expr[A]]
        case isf: ImplicitSearchFailure => report.throwError(isf.explanation)

    def inlineConstNull(using Quotes): Expr[Null] =
      import quotes.reflect.*
      Literal(NullConstant()).asInlineExprOf[Null]

    def inlineConstUnit(using Quotes): Expr[Unit] =
      import quotes.reflect.*
      Literal(UnitConstant()).asInlineExprOf[Unit]

    def inlineConst(value: Boolean)(using Quotes): Expr[Boolean] =
      import quotes.reflect.*
      Literal(BooleanConstant(value)).asInlineExprOf[Boolean]

    def inlineConst(value: Byte)(using Quotes): Expr[Byte] =
      import quotes.reflect.*
      Literal(ByteConstant(value)).asInlineExprOf[Byte]

    def inlineConst(value: Short)(using Quotes): Expr[Short] =
      import quotes.reflect.*
      Literal(ShortConstant(value)).asInlineExprOf[Short]

    def inlineConst(value: Int)(using Quotes): Expr[Int] =
      import quotes.reflect.*
      Literal(IntConstant(value)).asInlineExprOf[Int]

    def inlineConst(value: Long)(using Quotes): Expr[Long] =
      import quotes.reflect.*
      Literal(LongConstant(value)).asInlineExprOf[Long]

    def inlineConst(value: Float)(using Quotes): Expr[Float] =
      import quotes.reflect.*
      Literal(FloatConstant(value)).asInlineExprOf[Float]

    def inlineConst(value: Double)(using Quotes): Expr[Double] =
      import quotes.reflect.*
      Literal(DoubleConstant(value)).asInlineExprOf[Double]

    def inlineConst(value: Char)(using Quotes): Expr[Char] =
      import quotes.reflect.*
      Literal(CharConstant(value)).asInlineExprOf[Char]

    def inlineConst(value: String)(using Quotes): Expr[String] =
      inlineConstOrNull(value)

    def inlineConstOrNull(s: String | Null)(using Quotes): Expr[String | Null] =
      import quotes.reflect.*
      val const = if s == null then NullConstant() else StringConstant(s)
      Literal(const).asInlineExprOf[String | Null]

    // def productToTuple[P](using m: Mirror.ProductOf[P]): Expr[P => m.MirroredElemTypes] =
    //   1

    // def tupleToProduct[P](using m: Mirror.ProductOf[P]): Expr[m.MirroredElemTypes => P] =
    //   1
  }


  // ===================================================================================================================
  extension [A] (self: Expr[A]) {

    def showType(using Quotes): String =
      import quotes.reflect.*
      self.asTerm.tpe.show

    def tapShow()(using Quotes): Expr[A] =
      println(s"\n${self.show}\n")
      self

    def castTo[B](using Quotes, Type[A], Type[B]): Expr[B] =
      '{ $self.asInstanceOf[B] }

  }

  // ===================================================================================================================
  extension [F[_], A] (self: Expr[F[A]]) {

    inline def asExprOfF[B]: Expr[F[B]] =
      self.asInstanceOf[Expr[F[B]]]

    def castToF[B](using Quotes, Type[F], Type[A], Type[B]): Expr[F[B]] =
      '{ $self.asInstanceOf[F[B]] }

    inline def asExprOfFAny: Expr[F[Any]] =
      asExprOfF[Any]

    def castToFAny(using Quotes, Type[F], Type[A]): Expr[F[Any]] =
      castToF[Any]
  }

  // ===================================================================================================================
  extension [A](self: Type[A]) {

    def dealias(using Quotes): Type[A] =
      _type_dealias[A](using self)

    def summonOrError(using Quotes): Expr[A] =
      Expr.summonOrError[A](using self)
  }

  private def _type_dealias[A](using Type[A])(using Quotes): Type[A] =
    import quotes.reflect.*
    TypeRepr.of[A].dealias.asType.asInstanceOf[Type[A]]

  // ===================================================================================================================
  extension (using q: Quotes)(self: q.reflect.TypeRepr) {

    def asTypeTree: q.reflect.TypeTree =
      q.reflect.TypeTree.of(using self.asType)

    def exists: Boolean =
      import q.reflect.*
      self <:< TypeRepr.of[AnyRef] || self <:< TypeRepr.of[AnyVal]

    @targetName("summon_TypeRepr")
    def summon: Option[Expr[?]] =
      self.asType match
        case '[a] => Expr.summon[a]

    @targetName("summonOrError_TypeRepr")
    def summonOrError: Expr[?] =
      self.asType match
        case '[a] => Type.of[a].summonOrError
  }

  // ===================================================================================================================
  extension (using q: Quotes)(self: q.reflect.TypeTree) {

    inline def asType: Type[?] =
      self.tpe.asType

    @targetName("summon_TypeTree")
    def summon: Option[Expr[?]] =
      self.tpe.summon

    @targetName("summonOrError_TypeTree")
    def summonOrError: Expr[?] =
      self.tpe.summonOrError
  }

  // ===================================================================================================================
  extension (using q: Quotes)(self: q.reflect.Term) {

    def asConstant: q.reflect.Constant =
      import q.reflect.*
      self match
        case Literal(c) => c
        case x          => fail(s"Expected a constant literal, got: ${x.show} ")

    def simplify: q.reflect.Term =
      import q.reflect.*
      self match
        case Block(_, t) => t
        case _           => self

    def asInlineExprOf[A](using Type[A]): Expr[A] =
      import q.reflect.*
      Inlined(None, Nil, self).asExprOf[A]

    def implicitlyConvertTo[B](using Type[B]): Option[q.reflect.Term] =
      import quotes.reflect.*
      if self.tpe <:< TypeRepr.of[B] then
        Some(self)
      else
        self.tpe.asType match
          case '[t] =>
            Expr.summon[t => B].map { i =>
              val e = self.asExprOf[t]
              Expr.betaReduce('{ $i($e) }).asTerm
            }

    def implicitlyConvertToOrError[B](using Type[B]): q.reflect.Term =
      self.implicitlyConvertTo[B].getOrElse {
        val a = self.tpe.show
        val msg = s"Can't convert $a to ${Type.show[B]}"
        import quotes.reflect.*
        report.throwError(msg, Position.ofMacroExpansion)
      }
  }

  // ===================================================================================================================
  extension (using q: Quotes)(self: q.reflect.Symbol) {

    def getType: Option[q.reflect.TypeRepr] =
      import q.reflect.*
      self.tree match
        case ValDef(_, t, _)    => Some(t.tpe)
        case DefDef(_, _, t, _) => Some(t.tpe)
        case _                  => None

    inline def needType(inline descType: String): q.reflect.TypeRepr =
      self.needType(_ => descType)

    def needType(descType: q.reflect.Symbol => String): q.reflect.TypeRepr =
      self.getType.getOrElse(fail(s"Unable to determine type of ${descType(self)}"))

    /** Oldest returned first */
    def ownerPath: List[q.reflect.Symbol] =
      import q.reflect.*
      def loop(s: Symbol, acc: List[Symbol]): List[Symbol] =
        if s.isNoSymbol then
          acc
        else
          loop(s.maybeOwner, s :: acc)
      if self.isNoSymbol then
        self :: Nil
      else
        loop(self, Nil)

  }

}
