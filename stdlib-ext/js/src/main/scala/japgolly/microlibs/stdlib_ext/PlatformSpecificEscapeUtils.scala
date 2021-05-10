package japgolly.microlibs.stdlib_ext

// **********
// *        *
// *   JS   *
// *        *
// **********

import java.lang.{StringBuilder => JStringBuilder}

trait PlatformSpecificEscapeUtils { self: EscapeUtils.type =>

  override def quote(s: String): String =
    scala.scalajs.js.JSON.stringify(s)

  override def escape(s: String): String =
    if (s == null)
      null
    else {
      val q = quote(s)
      q.substring(1, q.length - 1)
    }

  override def appendQuoted(sb: JStringBuilder, s: String): Unit =
    sb.append(quote(s))

  override def appendEscaped(sb: JStringBuilder, s: String): Unit =
    sb.append(escape(s))

  override def appendQuoted(sb: StringBuilder, s: String): Unit =
    sb.append(quote(s))

  override def appendEscaped(sb: StringBuilder, s: String): Unit =
    sb.append(escape(s))

}
