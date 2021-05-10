package japgolly.microlibs.stdlib_ext

import japgolly.microlibs.testutil.TestUtil._
import java.lang.{StringBuilder => JStringBuilder}
import nyaya.gen.Gen
import sourcecode.Line
import utest._

object EscapeUtilsTest extends TestSuite {

  private def withSB(f: JStringBuilder => Unit): String = {
    val sb = new JStringBuilder
    f(sb)
    sb.toString
  }

  override def tests = Tests {

    "quoteAndEscape" - {
      def test(input: String, expected: String = null)(implicit l: Line): Unit = {
        val expect = Option(expected).getOrElse(input)
        val expectQ = "\"" + expect + "\""
        assertEq("escape", EscapeUtils.escape(input), expect)
        assertEq("quote", EscapeUtils.quote(input), expectQ)
        assertEq("escape sb", withSB(EscapeUtils.appendEscaped(_, input)), expect)
        assertEq("quote sb", withSB(EscapeUtils.appendQuoted(_, input)), expectQ)
      }
      "noNeed" - {
        "empty" - test("")
        "asciiMid" - {
          val exclude = "\\\"".toCharArray()
          val chars = (32 to 126).map(_.toChar).filterNot(exclude.contains).toVector
          test(Gen.shuffle(chars).sample().mkString)
        }
      }
      "needed" - {
        "whitelist" - test(" \" \\ \r \n \t \b ", " \\\" \\\\ \\r \\n \\t \\b ")
        "asciiLo"   - {
          val actual = (0 to 31).map(_.toChar).mkString
          val expect = "\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\b\\t\\n\\u000b\\f\\r\\u000e\\u000f\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001a\\u001b\\u001c\\u001d\\u001e\\u001f"
          test(actual, expect)
        }
        "asciiHi" - {
          val chars = (127 to 255).map(_.toChar).toVector
          test(Gen.shuffle(chars).sample().mkString)
        }
        "unicode" - test(Gen.unicode.map(c => if (c < 256) (c + 1000).toChar else c).string(128).sample())
      }
    }

  }
}