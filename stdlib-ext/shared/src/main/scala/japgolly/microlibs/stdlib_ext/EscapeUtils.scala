package japgolly.microlibs.stdlib_ext

import java.lang.{StringBuilder => JStringBuilder}

trait EscapeUtils {
  def quote(s: String): String
  def escape(s: String): String

  def appendQuoted(sb: JStringBuilder, s: String): Unit
  def appendEscaped(sb: JStringBuilder, s: String): Unit

  def appendQuoted(sb: StringBuilder, s: String): Unit
  def appendEscaped(sb: StringBuilder, s: String): Unit
}

object EscapeUtils extends EscapeUtils with PlatformSpecificEscapeUtils
