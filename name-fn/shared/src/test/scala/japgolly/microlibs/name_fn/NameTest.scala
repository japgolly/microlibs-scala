package japgolly.microlibs.name_fn

import japgolly.microlibs.name_fn.Name.Implicits._
import utest._

object NameTest extends TestSuite {

  override def tests = Tests {

    "nonStrict" - {
      "name" - {
        var i = 0
        val n: Name = s"i = ${ i += 1; i.toString }"
        assert(i == 0)
        assert(n.value == "i = 1")
        assert(i == 1)
        assert(n.value == "i = 1")
        assert(i == 1)
      }

      "nameFn" - {
        var i = 0
        val f: NameFn[Unit] = s"i = ${ i += 1; i.toString }"
        assert(i == 0)
        assert(f(None).value == "i = 1")
        assert(i == 1)
        assert(f(None).value == "i = 1")
        assert(i == 1)
        assert(f(Some(())).value == "i = 1")
        assert(i == 1)
      }
    }

    "pure" - {
      "name" - {
        val n: Name = "good"
        assertMatch(n) { case _: Name.Now => () }
      }
      "nameFn" - {
        val fn: NameFn[Unit] = "good"
        val a = fn(None)
        val b = fn(Some(()))
        assertMatch(a) { case _: Name.Now => () }
        assert(a eq b)
      }
    }
  }
}
