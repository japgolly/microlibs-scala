package japgolly.microlibs.compiletime

object InlineUtils:

  inline def warn(inline warning: String): Unit =
    ${ QuotingUtils.warn('warning) }

  inline def printCode(inline body: Any): body.type =
    println(showCode(body))
    body

  inline def printTasty(inline body: Any): body.type =
    println(showTasty(body))
    body

  // All of this is doulbed in TransparentInlineUtils

  inline def nonBlank(inline str: String): Option[String] =
    inline trim(str) match
      case "" => None
      case v  => Some[v.type](v)

  inline def replaceFirst(str: String, regex: String, repl: String): String =
    ${ QuotingUtils.replaceFirst('str, 'regex, 'repl) }

  inline def replaceAll(str: String, regex: String, repl: String): String =
    ${ QuotingUtils.replaceAll('str, 'regex, 'repl) }

  inline def trim(str: String): String =
    ${ QuotingUtils.trim('str) }

  inline def trimLowerCaseNonBlank(inline s: String): Option[String] =
    inline trim(toLowerCase(s)) match
      case "" => None
      case v  => Some[v.type](v)

  inline def trimLowerCaseNonBlank(inline o: Option[String]): Option[String] =
    inline o match
      case Some(v) => trimLowerCaseNonBlank(v)
      case None    => None

  inline def toLowerCase(str: String): String =
    ${ QuotingUtils.toLowerCase('str) }

  inline def toUpperCase(str: String): String =
    ${ QuotingUtils.toUpperCase('str) }

  inline def toInt(str: String): Int =
    ${ QuotingUtils.toInt('str) }

  inline def toLong(str: String): Long =
    ${ QuotingUtils.toLong('str) }

  inline def toBoolean(str: String): Boolean =
    ${ QuotingUtils.toBoolean('str) }

  inline def showCode(inline body: Any): String =
    ${ QuotingUtils.showCode('body) }

  inline def showTasty(inline body: Any): String =
    ${ QuotingUtils.showTasty('body) }
