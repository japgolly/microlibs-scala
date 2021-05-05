package japgolly.microlibs.macro_utils

import scala.quoted.*

object MacroEnv {

  inline def fail(msg: String)(using Quotes): Nothing =
    quotes.reflect.report.throwError(msg)

  // ===================================================================================================================
  extension [A] (self: Expr[A]) {

    def debugPrint()(using Quotes): Expr[A] =
      println(s"\n${self.show}\n")
      self
  }

  // ===================================================================================================================
  extension [F[_], A] (self: Expr[F[A]]) {

    inline def asFAny: Expr[F[Any]] =
      self.asInstanceOf[Expr[F[Any]]]

    def castToFAny(using Quotes, Type[F], Type[A]): Expr[F[Any]] =
      '{ $self.asInstanceOf[F[Any]] }
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

  }
}
