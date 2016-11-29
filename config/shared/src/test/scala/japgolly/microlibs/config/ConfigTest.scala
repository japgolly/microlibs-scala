package japgolly.microlibs.config

import japgolly.microlibs.testutil.TestUtil._
import scalaz.std.AllInstances._
import scalaz.syntax.applicative._
import scalaz.Scalaz.Id
import utest._
import ValueReader.Y._
import ValueReader.N._

object ConfigTest extends TestSuite {

  implicit def equalResultX[A] = scalaz.Equal.equalA[ResultX[A]]

  val src1 = Source.manual[Id]("S1")("i" -> "3", "s" -> "hey")
  val src2 = Source.manual[Id]("S2")("i" -> "300", "i2" -> "22", "s2" -> "ah")

  val srcs: Sources[Id] =
     src1 > src2


  override def tests = TestSuite {

    'findFirst -
      assertEq(Config.need[String]("s").run(srcs), ResultX.Success("hey"))

    'findSecond -
      assertEq(Config.need[Int]("i2").run(srcs), ResultX.Success(22))

    'notFound -
      assertEq(Config.get[Int]("notfound").run(srcs), ResultX.Success(Option.empty[Int]))

    'missing1 -
      assertEq(Config.need[Int]("missing").run(srcs), ResultX.QueryFailure(Map(Key("missing") -> None)))

    'missing2 -
      assertEq(
        (Config.need[Int]("no1") tuple Config.need[Int]("no2")).run(srcs),
        ResultX.QueryFailure(Map(Key("no1") -> None, Key("no2") -> None)))

    'valueFail1 -
      assertEq(
        Config.need[Int]("s").run(srcs),
        ResultX.QueryFailure(Map(Key("s") -> Some((src1.name, ConfigValue.Error("Not an Int.", Some("hey")))))))

    'valueFail2 -
      assertEq(
        Config.need[Int]("s2").run(srcs),
        ResultX.QueryFailure(Map(Key("s2") -> Some((src2.name, ConfigValue.Error("Not an Int.", Some("ah")))))))

  }
}
