package japgolly.microlibs.stdlib_ext

import java.lang.{StringBuilder => JStringBuilder}

trait EscapeUtils {

  /** `"a\n"` becomes `"\"a\\n\""` */
  def quote(s: String): String

  /** `"a\n"` becomes `"a\\n"` */
  def escape(s: String): String

  def appendQuoted(sb: JStringBuilder, s: String): Unit
  def appendEscaped(sb: JStringBuilder, s: String): Unit

  def appendQuoted(sb: StringBuilder, s: String): Unit
  def appendEscaped(sb: StringBuilder, s: String): Unit
}

object EscapeUtils extends EscapeUtils with PlatformSpecificEscapeUtils {

  def htmlEscape(s: String): String = {
    val sb = new JStringBuilder(s.length << 1)
    htmlAppendEscaped(sb, s)
    sb.toString
  }

  def htmlAppendEscaped(sb: JStringBuilder, s: String): Unit = {
    val chars = s.toCharArray()
    var i = 0
    var c = 'x'
    while (i < chars.length) {
      c = chars(i)
      if (c == '\"') sb.append("&quot;")
      else if (c == '<') sb.append("&lt;")
      else if (c == '>') sb.append("&gt;")
      else if (c == '&') sb.append("&amp;")
      else if (c == '\'') sb.append("&#39;")
      else sb.append(c)
      i += 1
    }
  }

  def htmlAppendEscaped(sb: StringBuilder, s: String): Unit = {
    val chars = s.toCharArray()
    var i = 0
    var c = 'x'
    while (i < chars.length) {
      c = chars(i)
      if (c == '\"') sb.append("&quot;")
      else if (c == '<') sb.append("&lt;")
      else if (c == '>') sb.append("&gt;")
      else if (c == '&') sb.append("&amp;")
      else if (c == '\'') sb.append("&#39;")
      else sb.append(c)
      i += 1
    }
  }
}
