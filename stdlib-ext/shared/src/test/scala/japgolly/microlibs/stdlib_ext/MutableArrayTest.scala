package japgolly.microlibs.stdlib_ext

import utest._

object MutableArrayTest extends TestSuite {

  def ji(i: Int) = java.lang.Integer.valueOf(i)

  override def tests = Tests {

    "anyRef" - {
      def m = MutableArray(List(2, 1, 3).map(ji))
      "init"   - assert(m.to(List).map(_.intValue) == List(2, 1, 3))
      "array"  - {
        val a: Array[java.lang.Integer] = m.array
        assert(a(0) == ji(2))
      }
      "map"    - assert(m.map(_.toString).to(List) == List("2", "1", "3"))
      "map2"   - assert(m.map(_ + 1).map(_.toString).to(List) == List("3", "2", "4"))
      "sort"   - assert(m.sort.to(List).map(_.intValue) == List(1, 2, 3))
      "mapAV"  - assert(m.map(_.intValue).to(List) == List(2, 1, 3))
    }

    "int" - {
      def m = MutableArray(List(2, 1, 3))
      "init"   - assert(m.to(List) == List(2, 1, 3))
      "array"  - {
        val a: Array[Int] = m.array
        assert(a.toList == List(2, 1, 3))
      }
      "arraySeq" - {
        val s = m.arraySeq
        assert(s.toList == List(2, 1, 3))
      }
      "map"    - assert(m.map(_.toString).to(List) == List("2", "1", "3"))
      "map2"   - assert(m.map(_ + 1).map(_.toString).to(List) == List("3", "2", "4"))
      "sort"   - assert(m.sort.to(List) == List(1, 2, 3))
      "sortArray" - {
        val a: Array[Int] = m.sort.array
        assert(a.toList == List(1, 2, 3))
      }
      "mapAR"  - assert(m.map(ji).to(List).map(_.intValue) == List(2, 1, 3))
    }

  }
}
