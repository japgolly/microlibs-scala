package japgolly.microlibs.nonempty

import utest._

object NonEmptySetTest extends TestSuite {

  override def tests = Tests {
    "hashCode" - {
      "equalsContract" - {
        val s1 = NonEmptySet(1, 2)
        val s2 = NonEmptySet(2, 1)

        assert(s1 == s2)
        assert(s1.hashCode == s2.hashCode)
      }

      "hashSetLookup" - {
        val s1 = NonEmptySet(1, 2)
        val s2 = NonEmptySet(2, 1)

        val set = Set(s1)
        assert(set.contains(s2))
      }
    }
  }
}
