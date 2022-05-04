package japgolly.microlibs.utils

import japgolly.microlibs.testutil.TestUtil._
import nyaya.gen.Gen
import org.scalacheck._
import utest._

object UtilsTest extends TestSuite {

  override def tests = Tests {

    "flattenArraySeqs" - {
      val gen = Gen.ascii.arraySeq(0 to 4)
      for {
        n <- 0 to 4
        s <- gen.arraySeq(n).samples().take(Math.pow(n, 2).toInt + 1)
      } {
        val actual = s.flatten
        val expect = Utils.flattenArraySeqs(s)
        assertEq(actual, expect)
      }
    }

    "partitionConsecutive" - {
      def test(in: Int*)(a: Int*)(b: Int*) =
        assertEq(Utils.partitionConsecutive(in.toList), (a.toList, b.toList))
      "1" - test()()()
      "2" - test(3)(3)()
      "3" - test(3, 4)(3, 4)()
      "4" - test(3, 5)(3)(5)
      "5" - test(3, 5, 6)(3)(5, 6)
      "6" - test(3, 4, 6)(3, 4)(6)
      "7" - test(3, 4, 5, 6)(3, 4, 5, 6)()
    }

    "separateByWhitespaceOrCommas" - {

      "manual" - assertEq(
        Utils.separateByWhitespaceOrCommas("omg  , k qq"),
        Vector(Right("omg"), Left("  , "), Right("k"), Left(" "), Right("qq")))

      "prop" - {
        val gen = Gen.chooseChar(' ', ",ab")
        for {
          n <- 0 to 6
          s <- gen.string(n).samples().take(Math.pow(n, 2).toInt + 1)
        } {
          val r = Utils.separateByWhitespaceOrCommas(s)
          assertEq(r.iterator.map(_.merge).mkString, s)
        }
      }
    }

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
