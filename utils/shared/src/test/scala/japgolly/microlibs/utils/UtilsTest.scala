package japgolly.microlibs.utils

import org.scalacheck._
import utest._

object UtilsTest extends TestSuite {

  override def tests = Tests {

    "quickStringExists" - {
      val x = "x"
      Prop.forAll { (ss: Set[String]) =>
        val f = Utils.quickStringExists(ss)
        (ss + "" + "123").toList
          .flatMap(s => s.drop(1) :: (s + x) :: (x + s + x) :: s :: Nil)
          .forall(s => {
            // println(s"${ss.contains(s)} / ${f(s)} -- [$s]")
            ss.contains(s) == f(s)
          })
      }.check()
    }

    "quickStringLookup" - {
      val x = "x"
      Prop.forAll { (m: Map[String, Int]) =>
        val f = Utils.quickStringLookup(m)
        (m.keySet + "" + "123").toList
          .flatMap(s => s.drop(1) :: (s + x) :: (x + s + x) :: s :: Nil)
          .forall(s => m.get(s) == f(s))
      }.check()
    }

  }
}
