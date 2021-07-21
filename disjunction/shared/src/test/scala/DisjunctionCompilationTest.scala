import japgolly.microlibs.disjunction._
import scala.annotation.nowarn

@nowarn
object DisjunctionCompilationTest {

  locally {
    val a                     = \/-(1)
    val b: Right[String, Int] = a
    val c: \/-[Int]           = b
  }

  locally {
    val a                    = -\/(1)
    val b: Left[Int, String] = a
    val c: -\/[Int]          = b
  }

  locally {
    val e: Either[Int, String] = Right("")
    e.leftMap(_ + 1)
  }

}
