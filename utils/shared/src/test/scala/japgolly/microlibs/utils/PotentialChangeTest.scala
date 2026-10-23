package japgolly.microlibs.utils

import cats.Eq
import japgolly.microlibs.utils.PotentialChange._
import utest._

object PotentialChangeTest extends TestSuite {

  implicit val eqInt: Eq[Int] = Eq.fromUniversalEquals

  override def tests = Tests {

    "filter" - {
      val s = Success(10)
      assert(s.filter(_ > 5) == Success(10))
      assert(s.filter(_ < 5) == Unchanged)

      val u: PotentialChange[String, Int] = Unchanged
      assert(u.filter(_ > 5) == Unchanged)

      val f: PotentialChange[String, Int] = Failure("err")
      assert(f.filter(_ > 5) == Failure("err"))
    }

    "ignore" - {
      val s = Success(10)
      assert(s.ignore(_ == 10) == Unchanged)
      assert(s.ignore(_ == 99) == Success(10))

      val u: PotentialChange[String, Int] = Unchanged
      assert(u.ignore(_ == 10) == Unchanged)

      val f: PotentialChange[String, Int] = Failure("err")
      assert(f.ignore(_ == 10) == Failure("err"))
    }

    "ignoreValue" - {
      val s = Success(10)
      assert(s.ignoreValue(10) == Unchanged)
      assert(s.ignoreValue(99) == Success(10))
    }

    "ignoreOption" - {
      val s = Success(10)
      assert(s.ignoreOption(Some(10)) == Unchanged)
      assert(s.ignoreOption(Some(99)) == Success(10))
      assert(s.ignoreOption(None) == Success(10))
    }

  }
}
