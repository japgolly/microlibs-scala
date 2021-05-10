package japgolly.microlibs.stdlib_ext

// ***********
// *         *
// *   JVM   *
// *         *
// ***********

import java.lang.{StringBuilder => JStringBuilder}

trait PlatformSpecificEscapeUtils { self: EscapeUtils.type =>

  override def quote(s: String): String = {
    val sb = new JStringBuilder(s.length + (s.length >> 1) + 2)
    appendQuoted(sb, s)
    sb.toString
  }

  override def appendQuoted(sb: JStringBuilder, s: String): Unit = {
    sb.append('"')
    appendEscaped(sb, s)
    sb.append('"')
    ()
  }
  override def appendQuoted(sb: StringBuilder, s: String): Unit = {
    sb.append('"')
    appendEscaped(sb, s)
    sb.append('"')
    ()
  }

  override def escape(s: String): String = {
    val sb = new JStringBuilder(s.length + (s.length >> 1))
    appendEscaped(sb, s)
    sb.toString
  }

  override def appendEscaped(sb: JStringBuilder, s: String): Unit = {
    val chars = s.toCharArray()
    var i = 0
    var c = 'x'
    while (i < chars.length) {
      c = chars(i)
      if (c == '\\') sb.append("\\\\")
      else if (c == '\"') sb.append("\\\"")
      else if (c == '\r') sb.append("\\r")
      else if (c == '\n') sb.append("\\n")
      else if (c == '\t') sb.append("\\t")
      else if (c == '\b') sb.append("\\b")
      else if (c == '\f') sb.append("\\f")
      else if (c < 32) sb.append("\\u%04x".format(c.toInt))
      else sb.append(c)
      i += 1
    }
  }
  override def appendEscaped(sb: StringBuilder, s: String): Unit = {
    val chars = s.toCharArray()
    var i = 0
    var c = 'x'
    while (i < chars.length) {
      c = chars(i)
      if (c == '\\') sb.append("\\\\")
      else if (c == '\"') sb.append("\\\"")
      else if (c == '\r') sb.append("\\r")
      else if (c == '\n') sb.append("\\n")
      else if (c == '\t') sb.append("\\t")
      else if (c == '\b') sb.append("\\b")
      else if (c == '\f') sb.append("\\f")
      else if (c < 32) sb.append("\\u%04x".format(c.toInt))
      else sb.append(c)
      i += 1
    }
  }

  def htmlEscape(s: String): String = {
    val sb = new JStringBuilder(s.length << 1)
    appendQuoted(sb, s)
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
