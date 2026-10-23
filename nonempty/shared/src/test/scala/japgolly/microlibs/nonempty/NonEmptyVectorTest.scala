package japgolly.microlibs.nonempty

import utest._

object NonEmptyVectorTest extends TestSuite {

  override def tests = Tests {
    "partitionB" - {
      "headPreserved" - {
        val v = NonEmptyVector(1, 2, 3)
        val (ts, fs) = v.partitionB(_ % 2 != 0)
        assert(ts == NonEmptyVector(1, 3))
        assert(fs == Vector(2))
      }
      "singleton" - {
        val v = NonEmptyVector(1)
        val (ts, fs) = v.partitionB(_ => true)
        assert(ts == NonEmptyVector(1))
        assert(fs == Vector.empty)
      }
    }

    "partitionD" - {
      "headPreserved" - {
        val v = NonEmptyVector(1, 2, 3)
        val res = v.partitionD(i => if (i % 2 != 0) Left(i) else Right(i))
        assert(res == Left((NonEmptyVector(1, 3), Vector(2))))
      }
      "singleton" - {
        val v = NonEmptyVector(1)
        val res = v.partitionD(i => Left(i))
        assert(res == Left((NonEmptyVector(1), Vector.empty)))
      }
    }
  }
}
