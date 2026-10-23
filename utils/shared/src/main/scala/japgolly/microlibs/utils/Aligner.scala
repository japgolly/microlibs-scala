package japgolly.microlibs.utils

object Aligner {

  def forStrings(): Mutable[String] =
    new Mutable(_.length)

  final class Mutable[I](f: I => Int) {
    private[this] var maxLen = 0

    def consider(i: I, plus: Int = 0): Unit = {
      val len = f(i) + plus
      if (len > maxLen)
        maxLen = len
    }

    def paddingSize(len: Int): Int =
      if (len >= maxLen) 0 else maxLen - len

    def padRight(s: String): String = {
      val p = paddingSize(s.length)
      if (p == 0)
        s
      else {
        val sb = new StringBuilder(maxLen)
        padRight(sb, s)
        sb.toString
      }
    }

    def padRight(sb: StringBuilder, s: String): Unit = {
      sb.append(s)
      var p = paddingSize(s.length)
      while (p > 0) {
        p -= 1
        sb append ' '
      }
    }

  }
}
