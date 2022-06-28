package japgolly.microlibs.compiletime

import java.util.regex.Pattern
import scala.quoted.*
import MacroEnv.*

object QuotingUtils:

  def warn(warning: Expr[String])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    report.warning(warning.valueOrAbort)
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

  def toLong(str: Expr[String])(using Quotes): Expr[Long] =
    str.value match
      case Some(s) =>
        try
          Expr.inlineConst(s.toLong)
        catch
          case _: Throwable => fail(s"Can't convert \"$s\" to a Long")
      case None =>
        '{ $str.toLong }

  def toBoolean(str: Expr[String])(using Quotes): Expr[Boolean] =
    str.value match
      case Some(s) =>
        try
          Expr.inlineConst(parseBooleanOrThrow(s))
        catch
          case t: Throwable => fail(t.getMessage)
      case None =>
        '{ parseBooleanOrThrow($str) }

  private val RegexTrue = Pattern.compile("^(?:t(?:rue)?|y(?:es)?|1|on|enabled?)$", Pattern.CASE_INSENSITIVE)
  private val RegexFalse = Pattern.compile("^(?:f(?:alse)?|n(?:o)?|0|off|disabled?)$", Pattern.CASE_INSENSITIVE)

  def parseBooleanOrThrow(s: String): Boolean =
    if (RegexTrue.matcher(s).matches)
      true
    else if (RegexFalse.matcher(s).matches)
      false
    else
      throw new RuntimeException(s"Can't parse \"$s\" as a Boolean")

  def showCode(e: Expr[Any])(using Quotes): Expr[String] =
    import quotes.reflect.*
    Expr.inlineConst(e.show)

  def showTasty(e: Expr[Any])(using Quotes): Expr[String] =
    import quotes.reflect.*
    Expr.inlineConst("" + e.asTerm)
