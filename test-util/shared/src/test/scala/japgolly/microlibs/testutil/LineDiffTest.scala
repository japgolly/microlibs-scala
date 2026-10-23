package japgolly.microlibs.testutil

import utest._

object LineDiffTest extends TestSuite {
  import TestUtil._

  private def diff(s1: Seq[String], s2: Seq[String]): String =
    LineDiff(s1, s2).diffCustom(" ", "-", "+", "")

  override def tests = Tests {

    "empty" - {
      "both" - assertEq(diff(Nil, Nil), "")
      "left" - assertEq(diff(Nil, Seq("a", "b")), "+a\n+b")
      "right" - assertEq(diff(Seq("a", "b"), Nil), "-a\n-b")
    }

    "stringInput" - {
      val d = LineDiff("a\nb", "a\nc").diffCustom(" ", "-", "+", "")
      assertEq(d, " a\n-b\n+c")
    }

    "identical" - {
      val lines = Seq("a", "b", "c")
      assertEq(diff(lines, lines), " a\n b\n c")
    }

    "disjoint" - {
      val lines1 = Seq("a", "b")
      val lines2 = Seq("x", "y")
      assertEq(diff(lines1, lines2), "-a\n-b\n+x\n+y")
    }

    "patienceDiff" - {
      "middleInsert" - {
        val lines1 = Seq("a", "b", "c")
        val lines2 = Seq("a", "b", "INS", "c")
        assertEq(diff(lines1, lines2), " a\n b\n+INS\n c")
      }

      "middleDelete" - {
        val lines1 = Seq("a", "b", "DEL", "c")
        val lines2 = Seq("a", "b", "c")
        assertEq(diff(lines1, lines2), " a\n b\n-DEL\n c")
      }

      "middleModify" - {
        val lines1 = Seq("a", "OLD", "b")
        val lines2 = Seq("a", "NEW", "b")
        assertEq(diff(lines1, lines2), " a\n-OLD\n+NEW\n b")
      }

      "multipleGaps" - {
        val l1 = Seq("1", "2", "3", "4", "5")
        val l2 = Seq("1", "3", "5")
        assertEq(diff(l1, l2), " 1\n-2\n 3\n-4\n 5")
      }
    }

    "startMatches" - {
      "nonUniqueStartMatches" - {
        val lines1 = Seq("dup", "dup", "a")
        val lines2 = Seq("dup", "dup", "b")
        assertEq(diff(lines1, lines2), " dup\n dup\n-a\n+b")
      }

      "allMatch" - {
        val lines = Seq("dup", "dup")
        assertEq(diff(lines, lines), " dup\n dup")
      }
    }

    "endMatches" - {
      "singleMatchWithoutPredecessorMatch" - {
        // Line 3 is "dup" in both sequences, but line 2 differs ("b" vs "y").
        // lines1 and lines2 contain duplicate lines so uniqueLCS is empty.
        // Due to bug #24, findEndMatches checked lines(pos - 1) and skipped line 3 entirely.
        val lines1 = Seq("a", "dup", "b", "dup")
        val lines2 = Seq("x", "dup", "y", "dup")
        val expected =
          """-a
            |+x
            | dup
            |-b
            |+y
            | dup""".stripMargin
        assertEq(diff(lines1, lines2), expected)
      }

      "multipleMatchesAtEnd" - {
        // Lines 1 and 2 are equal ("dup"), while line 0 differs.
        // Due to bug #24, the matched index at (bounds1._2 - 1) caused the predecessor to be dropped,
        // and matches were accumulated in descending order.
        val lines1 = Seq("a", "dup", "dup")
        val lines2 = Seq("x", "dup", "dup")
        val expected =
          """-a
            |+x
            | dup
            | dup""".stripMargin
        assertEq(diff(lines1, lines2), expected)
      }

      "threeMatchesAtEnd" - {
        val lines1 = Seq("a", "dup", "dup", "dup")
        val lines2 = Seq("x", "dup", "dup", "dup")
        val expected =
          """-a
            |+x
            | dup
            | dup
            | dup""".stripMargin
        assertEq(diff(lines1, lines2), expected)
      }

      "insertAtStartWithEndMatches" - {
        val lines1 = Seq("dup", "dup")
        val lines2 = Seq("x", "dup", "dup")
        assertEq(diff(lines1, lines2), "+x\n dup\n dup")
      }

      "deleteAtStartWithEndMatches" - {
        val lines1 = Seq("x", "dup", "dup")
        val lines2 = Seq("dup", "dup")
        assertEq(diff(lines1, lines2), "-x\n dup\n dup")
      }
    }

    "startAndEndMatches" - {
      val lines1 = Seq("dup", "dup", "a", "dup", "dup")
      val lines2 = Seq("dup", "dup", "b", "dup", "dup")
      assertEq(diff(lines1, lines2), " dup\n dup\n-a\n+b\n dup\n dup")
    }

    "trailingWhitespace" - {
      "none" - assertEq(LineDiff.highlightTrailingWhitespace("hello"), "hello")
      "spaces" - assertEq(LineDiff.highlightTrailingWhitespace("hello  "), s"hello${scala.Console.RED_B}  ")
      "onlyWhitespace" - assertEq(LineDiff.highlightTrailingWhitespace("  "), s"${scala.Console.RED_B}  ")
      "empty" - assertEq(LineDiff.highlightTrailingWhitespace(""), "")
    }

    "colours" - {
      val d = LineDiff("a", "b")
      assert(d.diffColoured.contains("-a"))
      assert(d.diffColoured.contains("+b"))
      assert(d.expectActualColoured.contains("-e| a"))
      assert(d.expectActualColoured.contains("+a| b"))
    }
  }
}
