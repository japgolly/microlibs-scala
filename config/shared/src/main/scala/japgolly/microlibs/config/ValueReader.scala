package japgolly.microlibs.config

import japgolly.microlibs.stdlib_ext._
import java.util.regex.Pattern
import scalaz.{-\/, \/, \/-}

final case class ValueReader[A](read: ConfigValue.Found => String \/ A) extends AnyVal {
  def map[B](f: A => B): ValueReader[B] =
    ValueReader(read(_) map f)

  def mapAttempt[B](f: A => String \/ B): ValueReader[B] =
    ValueReader(read(_) flatMap f)

  def flatMap[B](f: A => ValueReader[B]): ValueReader[B] =
    ValueReader(v => read(v).flatMap(f(_) read v))
}

object ValueReader {
  object X {
    implicit val readString: ValueReader[String] =
      ValueReader(v => \/-(v.value))
  }
  object Y {
    implicit val readString: ValueReader[String] =
      X.readString.map(_.trim.replaceFirst("\\s*#.*$", ""))
  }
  object N {
    implicit def readInt(implicit s: ValueReader[String]): ValueReader[Int] =
      s.mapAttempt {
        case ParseInt(i) => \/-(i)
        case _ => -\/("Int expected.")
      }

    implicit def readLong(s: ValueReader[String]): ValueReader[Long] =
      s.mapAttempt {
        case ParseLong(l) => \/-(l)
        case _ => -\/("Long expected.")
      }

    private val RegexTrue = Pattern.compile("^(?:t(?:rue)?|y(?:es)?|1|on|enabled?)$", Pattern.CASE_INSENSITIVE)
    private val RegexFalse = Pattern.compile("^(?:f(?:alse)?|n(?:o)?|0|off|disabled?)$", Pattern.CASE_INSENSITIVE)

    implicit def readBoolean(s: ValueReader[String]): ValueReader[Boolean] =
      s.mapAttempt(s =>
        if (RegexTrue.matcher(s).matches)
          \/-(true)
        else if (RegexFalse.matcher(s).matches)
          \/-(false)
        else
          -\/("Boolean expected.")
      )
  }
}

