package japgolly.microlibs.stdlib_ext

import utest._
import StdlibExt._

object StdlibExtTest extends TestSuite {
  override def tests = Tests {

    "indent(int)" - {
      assert("a".indent(2) == "  a")
      assert("a\nb".indent(2) == "  a\n  b")
      assert("a\n  b".indent(2) == "  a\n    b")
    }

    'vectorInsertBefore {
      for {
        vs <- List(Vector(), Vector(1), Vector(1, 2, 3))
        i  <- -2  to vs.length + 2
      } {
        val r = vs.insertBefore(i, 666)
        if (i >= 0 && i <= vs.length) {
          assert(r.isDefined)
          val n = r.get
          assert(n(i) == 666)
          assert(n.filterNot(_ == 666) == vs)
        } else
          assert(r.isEmpty)
      }
    }

    "regex.collectAllIn" - {
      val r = "[0-9]".r
      def test(input: String)(expectedMatches: Int*): Unit = {
        val (init, last) = r.collectAllIn(input)
        val all = (init.flatMap(x => x._1 :: x._2.group(0) :: Nil) :+ last).mkString("")
        val matches = init.map(_._2.group(0).toInt)
        assert(all == input, matches == expectedMatches.toList)
      }

      "a" - test("a")()
      "1" - test("1")(1)
      "1 2" - test("1 2")(1, 2)
      "1 a 2" - test("1 a 2")(1, 2)
      "1 a 2 y" - test("1 a 2 y")(1, 2)
      "x 1 a 2" - test("x 1 a 2")(1, 2)
      "x 1 a 2 y" - test("x 1 a 2 y")(1, 2)
      "xx 11 aa 22 yy" - test("xx 11 aa 22 yy")(1, 1, 2, 2)
    }
  }
}
