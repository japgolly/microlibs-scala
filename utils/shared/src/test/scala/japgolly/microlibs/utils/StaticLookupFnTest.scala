package japgolly.microlibs.utils

import utest._

object StaticLookupFnTest extends TestSuite {

  private sealed abstract class A(val key: Int)
  private case object A0 extends A(0)
  private case object A1 extends A(1)
  private case object A3 extends A(3)
  private case object A4 extends A(4)
  private case object A6 extends A(6)
  private case object Dup extends A(1)

  private val values = List[A](A0, A1, A3, A4, A6)

  private val badKeys = List[Int](-1, 2, 5, 7, 500)

  private def assertNoKey[@specialized(Int) K, V](f: K => V)(k: K): Unit = {
    intercept[NoSuchElementException]({f(k); ()})
    ()
  }

  private def assertDup[@specialized(Int) K, V](dsl: StaticLookupFn.ArrayDsl[K, V]): Unit = {
    intercept[ExceptionInInitializerError]({dsl.total; ()})
    intercept[ExceptionInInitializerError]({dsl.toOption; ()})
    intercept[ExceptionInInitializerError]({dsl.toEither(_ => ()); ()})
    ()
  }

  override def tests = Tests {

    'array {
      val dsl = StaticLookupFn.useArrayBy(values)(_.key)

      'dup - assertDup(StaticLookupFn.useArrayBy(Dup :: values)(_.key))

      'total - {
        val f = dsl.total
        values.foreach(a => f(a.key) ==> a)
        badKeys.foreach(assertNoKey(f))
      }

      'option - {
        val f = dsl.toOption
        values.foreach(a => f(a.key) ==> Some(a))
        badKeys.foreach(f(_) ==> None)
      }

      'either - {
        val f = dsl.toEither(identity)
        values.foreach(a => f(a.key) ==> Right(a))
        badKeys.foreach(k => f(k) ==> Left(k))
      }
    }

    'map {
      val dsl = StaticLookupFn.useMapBy(values)(_.key)

      'dup - assertDup(StaticLookupFn.useArrayBy(Dup :: values)(_.key))

      'total - {
        val f = dsl.total
        values.foreach(a => f(a.key) ==> a)
        badKeys.foreach(assertNoKey(f))
      }

      'option - {
        val f = dsl.toOption
        values.foreach(a => f(a.key) ==> Some(a))
        badKeys.foreach(f(_) ==> None)
      }

      'either - {
        val f = dsl.toEither(identity)
        values.foreach(a => f(a.key) ==> Right(a))
        badKeys.foreach(k => f(k) ==> Left(k))
      }
    }

    'mapTiny {
      // Because I may have some optimisations to match Scala's optimised representations of Maps with <= 4 keys
      val values = StaticLookupFnTest.this.values.take(3)
      val dsl = StaticLookupFn.useMapBy(values)(_.key)

      'dup - assertDup(StaticLookupFn.useArrayBy(Dup :: values)(_.key))

      'total - {
        val f = dsl.total
        values.foreach(a => f(a.key) ==> a)
        badKeys.foreach(assertNoKey(f))
      }

      'option - {
        val f = dsl.toOption
        values.foreach(a => f(a.key) ==> Some(a))
        badKeys.foreach(f(_) ==> None)
      }

      'either - {
        val f = dsl.toEither(identity)
        values.foreach(a => f(a.key) ==> Right(a))
        badKeys.foreach(k => f(k) ==> Left(k))
      }
    }

  }
}
