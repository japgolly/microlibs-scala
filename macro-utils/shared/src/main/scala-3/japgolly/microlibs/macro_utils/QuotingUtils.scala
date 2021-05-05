package japgolly.microlibs.macro_utils

import scala.quoted.*
import MacroEnv.*

object QuotingUtils:

  def warn(warning: Expr[String])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    report.warning(warning.valueOrError)
    Expr.inlineConstUnit

  def replaceFirst(str: Expr[String], regex: Expr[String], repl: Expr[String])(using Quotes): Expr[String] =
    (str.value, regex.value, repl.value) match
      case (Some(n), Some(r), Some(p)) => Expr.inlineConst(n.replaceFirst(r, p))
      case _                           => '{ $str.replaceFirst($regex, $repl) }

  def replaceAll(str: Expr[String], regex: Expr[String], repl: Expr[String])(using Quotes): Expr[String] =
    (str.value, regex.value, repl.value) match
      case (Some(n), Some(r), Some(p)) => Expr.inlineConst(n.replaceAll(r, p))
      case _                           => '{ $str.replaceAll($regex, $repl) }

  def trim(str: Expr[String])(using Quotes): Expr[String] =
    str.value match
      case Some(s) => Expr.inlineConst(s.trim)
      case None    => '{ $str.trim }

  def toLowerCase(str: Expr[String])(using Quotes): Expr[String] =
    str.value match
      case Some(s) => Expr.inlineConst(s.toLowerCase)
      case None    => '{ $str.toLowerCase }

  def toUpperCase(str: Expr[String])(using Quotes): Expr[String] =
    str.value match
      case Some(s) => Expr.inlineConst(s.toUpperCase)
      case None    => '{ $str.toUpperCase }

  def toInt(str: Expr[String])(using Quotes): Expr[Int] =
    str.value match
      case Some(s) =>
        try
          Expr.inlineConst(s.toInt)
        catch
          case _: Throwable => fail(s"Can't convert \"$s\" to an Int")
      case None =>
        '{ $str.toInt }

