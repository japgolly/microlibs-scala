package japgolly.microlibs.compiletime

object TransparentInlineUtils:

  transparent inline def nonBlank(inline str: String): Option[String] =
    inline trim(str) match
      case "" => None
      case v  => Some[v.type](v)

  transparent inline def replaceFirst(str: String, regex: String, repl: String): String =
    ${ QuotingUtils.replaceFirst('str, 'regex, 'repl) }

  transparent inline def replaceAll(str: String, regex: String, repl: String): String =
    ${ QuotingUtils.replaceAll('str, 'regex, 'repl) }

  transparent inline def trim(str: String): String =
    ${ QuotingUtils.trim('str) }

  transparent inline def trimLowerCaseNonBlank(inline s: String): Option[String] =
    inline trim(toLowerCase(s)) match
      case "" => None
      case v  => Some[v.type](v)

  transparent inline def trimLowerCaseNonBlank(inline o: Option[String]): Option[String] =
    inline o match
      case Some(v) => trimLowerCaseNonBlank(v)
      case None    => None

  transparent inline def toLowerCase(str: String): String =
    ${ QuotingUtils.toLowerCase('str) }

  transparent inline def toUpperCase(str: String): String =
    ${ QuotingUtils.toUpperCase('str) }

  transparent inline def toInt(str: String): Int =
    ${ QuotingUtils.toInt('str) }

  transparent inline def toLong(str: String): Long =
    ${ QuotingUtils.toLong('str) }

  transparent inline def toBoolean(str: String): Boolean =
    ${ QuotingUtils.toBoolean('str) }

  transparent inline def showCode(inline body: Any): String =
    ${ QuotingUtils.showCode('body) }

  transparent inline def showTasty(inline body: Any): String =
    ${ QuotingUtils.showTasty('body) }
