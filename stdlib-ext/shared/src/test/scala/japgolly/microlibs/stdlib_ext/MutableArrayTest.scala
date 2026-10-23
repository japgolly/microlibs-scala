package japgolly.microlibs.stdlib_ext

import scala.collection.immutable.ArraySeq
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
        assert(a.map(_.intValue).toList == List(2, 1, 3))
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
        val s: ArraySeq[Int] = m.arraySeq
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

    "empty" - {
      def m = MutableArray(List.empty[Int])
      "length"     - assert(m.length == 0)
      "isEmpty"    - assert(m.isEmpty)
      "nonEmpty"   - assert(!m.nonEmpty)
      "toString"   - assert(m.toString == "MutableArray[]")
      "array"      - assert(m.array.length == 0)
      "arraySeq"   - assert(m.arraySeq.isEmpty)
      "iterator"   - assert(m.iterator().isEmpty)
      "mkString1"  - assert(m.mkString == "")
      "mkString2"  - assert(m.mkString(",") == "")
      "mkString3"  - assert(m.mkString("[", ",", "]") == "[]")
      "sort"       - assert(m.sort.to(List) == Nil)
      "sortBy"     - assert(m.sortBy(identity).to(List) == Nil)
      "sortSch"    - assert(m.sortBySchwartzian(identity).to(List) == Nil)
    }

    "properties" - {
      val m = MutableArray(List("a", "b", "c"))
      "length"     - assert(m.length == 3)
      "isEmpty"    - assert(!m.isEmpty)
      "nonEmpty"   - assert(m.nonEmpty)
      "toString"   - assert(m.toString == "MutableArray[a, b, c]")
      "widen"      - {
        val mw: MutableArray[CharSequence] = m.widen[CharSequence]
        assert(mw.to(List) == List("a", "b", "c"))
      }
    }

    "mkString" - {
      "default" - {
        val m = MutableArray(List(1, 2, 3))
        assert(m.mkString == "123")
        assert(m.mkString(", ") == "1, 2, 3")
        assert(m.mkString("<", ", ", ">") == "<1, 2, 3>")
      }
      "withPendingMap" - {
        val m = MutableArray(List(1, 2, 3)).map(_ * 2)
        assert(m.mkString == "246")
        assert(m.mkString(", ") == "2, 4, 6")
        assert(m.mkString("<", ", ", ">") == "<2, 4, 6>")
      }
    }

    "viewAndIterator" - {
      val m = MutableArray(List("a", "b", "c"))
      "view"       - assert(m.view.map(_ + "!").toList == List("a!", "b!", "c!"))
      "iterator"   - assert(m.iterator().toList == List("a", "b", "c"))
      "toVector"   - assert(m.to(Vector) == Vector("a", "b", "c"))
      "withMap"    - {
        val m2 = m.map(_.toUpperCase)
        assert(m2.iterator().toList == List("A", "B", "C"))
        assert(m2.view.toList == List("A", "B", "C"))
        assert(m2.to(Vector) == Vector("A", "B", "C"))
      }
    }

    "sorting" - {
      "sortBy" - {
        val m = MutableArray(List("apple", "pie", "at"))
        assert(m.sortBy(_.length).to(List) == List("at", "pie", "apple"))
      }
      "sortBySchwartzian" - {
        val m = MutableArray(List("apple", "pie", "at"))
        assert(m.sortBySchwartzian(_.length).to(List) == List("at", "pie", "apple"))
      }
      "sortBySchwartzianPrimitives" - {
        val m = MutableArray(List(3, 1, 2))
        assert(m.sortBySchwartzian(-_).to(List) == List(3, 2, 1))
      }
    }

    "companionMap" - {
      val m = MutableArray.map(List(1, 2, 3))(_ * 10)
      assert(m.to(List) == List(10, 20, 30))
    }

    "primitives" - {
      "boolean" - {
        val a: Array[Boolean] = MutableArray(List(true, false, true)).array
        assert(a.toList == List(true, false, true))
      }
      "byte" - {
        val a: Array[Byte] = MutableArray(List(1.toByte, 2.toByte)).array
        assert(a.toList == List(1.toByte, 2.toByte))
      }
      "char" - {
        val a: Array[Char] = MutableArray(List('a', 'b', 'c')).array
        assert(a.toList == List('a', 'b', 'c'))
      }
      "short" - {
        val a: Array[Short] = MutableArray(List(1.toShort, 2.toShort)).array
        assert(a.toList == List(1.toShort, 2.toShort))
      }
      "int" - {
        val a: Array[Int] = MutableArray(List(1, 2, 3)).array
        assert(a.toList == List(1, 2, 3))
      }
      "long" - {
        val a: Array[Long] = MutableArray(List(1L, 2L, 3L)).array
        assert(a.toList == List(1L, 2L, 3L))
      }
      "float" - {
        val a: Array[Float] = MutableArray(List(1.0f, 2.0f)).array
        assert(a.toList == List(1.0f, 2.0f))
      }
      "double" - {
        val a: Array[Double] = MutableArray(List(1.0, 2.0)).array
        assert(a.toList == List(1.0, 2.0))
      }
      "unit" - {
        val a: Array[Unit] = MutableArray(List((), ())).array
        assert(a.length == 2)
      }
    }

    "refTypes" - {
      "string" - {
        val a: Array[String] = MutableArray(List("hello", "world")).array
        assert(a.toList == List("hello", "world"))
      }
      "stringArraySeq" - {
        val s: ArraySeq[String] = MutableArray(List("hello", "world")).arraySeq
        assert(s.toList == List("hello", "world"))
      }
      "anyRef" - {
        val o1 = "first"
        val o2 = "second"
        val a: Array[AnyRef] = MutableArray(List(o1: AnyRef, o2: AnyRef)).array
        assert(a.toList == List(o1, o2))
      }
      "any" - {
        val a: Array[Any] = MutableArray(List(1: Any, "two": Any)).array
        assert(a.length == 2)
        assert(a(0) == 1)
        assert(a(1) == "two")
      }
      "untyped" - {
        def genericLength[A](m: MutableArray[A]): Int = {
          val a: Array[A] = m.array
          a.length
        }
        val len = genericLength(MutableArray(List(1, 2)))
        assert(len == 2)
      }
    }

    "pendingMap" - {
      "chaining" - {
        val m = MutableArray(List(1, 2, 3))
        val m2 = m.map(_ + 1).map(_ * 2).map(_.toString)
        assert(m2.to(List) == List("4", "6", "8"))
      }
      "toIteratorWithoutArray" - {
        val m = MutableArray(List(1, 2, 3)).map(_ * 10)
        assert(m.iterator().toList == List(10, 20, 30))
      }
      "toArray" - {
        val m = MutableArray(List(1, 2, 3)).map(_ * 10)
        val a: Array[Int] = m.array
        assert(a.toList == List(10, 20, 30))
      }
      "toSort" - {
        val m = MutableArray(List(2, 1, 3)).map(_ * 10)
        assert(m.sort.to(List) == List(10, 20, 30))
      }
    }

  }
}
