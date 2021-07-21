package japgolly.microlibs.stdlib_ext

import java.time.Duration
import utest._

object StdlibExtTest extends TestSuite {
  import StdlibExt._

  private def assertEq[A](a: A, e: A) =
    assert(a == e)

  override def tests = Tests {

    "indent(int)" - {
      assert("a".indent(2) == "  a")
      assert("a\nb".indent(2) == "  a\n  b")
      assert("a\n  b".indent(2) == "  a\n    b")
    }

    "unindent" - {

      "equal" - {
        assertEq(
          """  omfg
            |
            |  good
            |""".stripMargin.unindent(2),
          """omfg
            |
            |good
            |""".stripMargin,
        )
      }

      "oneUnder" - {
        assertEq(
          """  omfg
            |
            | good
            |""".stripMargin.unindent(2),
          """ omfg
            |
            |good
            |""".stripMargin,
        )
      }

      "oneOver" - {
        assertEq(
          """  omfg
            |
            |   good
            |""".stripMargin.unindent(2),
          """omfg
            |
            | good
            |""".stripMargin,
        )
      }

      "allUnder" - {
        assertEq(
          """ omfg
            |
            | good
            |""".stripMargin.unindent(2),
          """omfg
            |
            |good
            |""".stripMargin,
        )
      }

      "allOver" - {
        assertEq(
          """   omfg
            |
            |   good
            |""".stripMargin.unindent(2),
          """ omfg
            |
            | good
            |""".stripMargin,
        )
      }

    }

    "vectorInsertBefore" - {
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

    "duration" - {
      "toSeconds" - {
        "pos" - {
          val d = Duration.ofMillis(1100) plus Duration.ofNanos(9002003L)
          d.asSeconds ==> 1.109002003
        }
        "neg" - {
          val d = Duration.ofMillis(-1100) minus Duration.ofNanos(9002003L)
          d.asSeconds ==> -1.109002003
        }
      }

      "conciseDesc" - {
        def test(sec: Double, ns: Long, expect: String): Unit = {
          val pos = Duration.ofSeconds(sec.toLong).plus(Duration.ofNanos(ns))
          pos.conciseDesc ==> expect

          val neg = pos.negated()
          neg.conciseDesc ==> s"-$expect"
        }

        "ns1" - test(0,   1,   "1 ns")
        "ns2" - test(0,  11,  "11 ns")
        "ns3" - test(0, 111, "111 ns")

        "us1" - test(0,   1001,   "1 us")
        "us2" - test(0,  11001,  "11 us")
        "us3" - test(0, 111001, "111 us")

        "ms1" - test(0,   1001001,   "1 ms")
        "ms2" - test(0,  11001001,  "11 ms")
        "ms3" - test(0, 111001001, "111 ms")

        "sec1" - test( 0, 1001001001,  "1.0 sec")
        "sec2" - test( 0, 1901001001,  "1.9 sec")
        "sec3" - test(59,  901001001, "59.9 sec")

        "min1" - test( 1.09 * 60, 3,  "1.1 min")
        "min2" - test(12.94 * 60, 3, "12.9 min")
        "min3" - test(59.88 * 60, 3, "59.9 min")

        "hr1" - test(1.02 * 3600, 3, "1.02 hr")
        "hr2" - test(23 * 3600, 3, "23.00 hr")

        "day1" - test(36 * 3600, 3, "1.50 days")
        "day2" - test(48 * 3600, 3, "2.00 days")
      }
    }
  }
}
