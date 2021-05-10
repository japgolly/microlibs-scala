package japgolly.microlibs.compiletime

object InlineUtils:

  inline def warn(inline warning: String): Unit =
    ${ QuotingUtils.warn('warning) }

  transparent inline def replaceFirst(str: String, regex: String, repl: String): String =
    ${ QuotingUtils.replaceFirst('str, 'regex, 'repl) }

  transparent inline def replaceAll(str: String, regex: String, repl: String): String =
    ${ QuotingUtils.replaceAll('str, 'regex, 'repl) }

  transparent inline def trim(str: String): String =
    ${ QuotingUtils.trim('str) }

  transparent inline def toLowerCase(str: String): String =
    ${ QuotingUtils.toLowerCase('str) }

  transparent inline def toUpperCase(str: String): String =
    ${ QuotingUtils.toUpperCase('str) }

  transparent inline def toInt(str: String): Int =
    ${ QuotingUtils.toInt('str) }
