package japgolly.microlibs.nonempty

import utest._

object NonEmptyArraySeqTest extends TestSuite {

  override def tests = Tests {
    "intercalate" - {
      val as = NonEmptyArraySeq(1, 2, 3)
      val actual = as.intercalate(0)
      val expect = NonEmptyArraySeq(1, 0, 2, 0, 3)
      assert(actual == expect)
    }

    "intercalateF" - {
      val as = NonEmptyArraySeq("a", "b", "c")
      val actual = as.intercalateF("-")(_.toUpperCase)
      val expect = NonEmptyArraySeq("A", "-", "B", "-", "C")
      assert(actual == expect)
    }
  }
}
