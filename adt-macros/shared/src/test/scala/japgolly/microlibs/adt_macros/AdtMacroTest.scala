package japgolly.microlibs.adt_macros

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import utest._
import aa._
import AdtMacros._
import japgolly.univeq.UnivEq

object AdtMacroTest extends TestSuite {

  def assertOrderedNEV[A](actual: NonEmptyVector[A], expect: NonEmptyVector[A]): Unit =
    assert(actual == expect)

  def assertOrderedNEV[A](actual: NonEmptyVector[A])(e1: A, eN: A*): Unit =
    assertOrderedNEV(actual, NonEmptyVector(e1, eN.toVector))

  def assertUnorderedNEV[A](actual: NonEmptyVector[A], expect: NonEmptyVector[A]): Unit = {
    val norm: NonEmptyVector[A] => NonEmptyVector[A] = _.sortBy(_.toString)
    assertOrderedNEV(norm(actual), norm(expect))
  }

  def assertUnorderedNEV[A](actual: NonEmptyVector[A])(e1: A, eN: A*): Unit =
    assertUnorderedNEV(actual, NonEmptyVector(e1, eN.toVector))

  def assertFail(e: CompileError) = e.msg

  implicit def univEqS3: UnivEq[MonoS3] = UnivEq.derive[MonoS3]

  override def tests = Tests {

    "adtValues" - {
//      's1i - assertUnorderedNEV(MonoS1.Values)(MonoS1.A)
//      's3i - assertUnorderedNEV(MonoS3.Values)(MonoS3.A, MonoS3.B, MonoS3.C)
      "s1" - assertUnorderedNEV(adtValues[MonoS1])(MonoS1.A)
      "s3" - assertUnorderedNEV(adtValues[MonoS3])(MonoS3.A, MonoS3.B, MonoS3.C)
      "d1" - assertFail(compileError("adtValues[MonoD1]"))
      "d2" - assertFail(compileError("adtValues[MonoD2]"))
      "unsealed" - assertFail(compileError("adtValues[Unsealed]"))

      // TODO Pending https://github.com/lampepfl/dotty/issues/11765
      // "emptySubtype" - {
      //   import EmptySubType._
      //   assertUnorderedNEV(adtValues[A])(D)
      // }
    }

    "valuesForAdt" - {
      "ok" - {
        import MonoD._
        assertUnorderedNEV(valuesForAdt[MonoD, String] {
          case _: A => "A"
          case _: B => "B"
          case C    => "C"
          case _: D => "D"
        })("A", "B", "C", "D")
      }
      // TODO Pending https://github.com/lampepfl/dotty/issues/11765
      // "sub" - {
      //   import MonoSub._
      //   assertUnorderedNEV(valuesForAdt[MonoSub, String] {
      //     case A => "A"
      //     case _: B => "B"
      //   })("A", "B")
      // }
      "missing" - {
        import MonoD._
        assertFail(compileError("valuesForAdt[MonoD, String] {case _: A => \"A\"}"))
      }
      "dup" - {
        import MonoD._
        assertFail(compileError(
          """
            valuesForAdt[MonoD, String] {
              case _: A => "A1"
              case _: A => "A2"
              case _: B => "B"
              case C    => "C"
              case _: D => "D"
            }
          """))
      }
      "extra" - {
        import MonoD._
        assertFail(compileError(
          """
            valuesForAdt[MonoD, String] {
              case _: A => "A"
              case _: Int => "I"
              case _: B => "B"
              case C    => "C"
              case _: D => "D"
            }
          """))
      }
    }

    "adtIso" - {
      val (mc, cm, ms, cs) = adtIso[MonoS3, Char] {
        case MonoS3.A => 'a'
        case MonoS3.B => 'b'
        case MonoS3.C => 'c'
      }
      assertUnorderedNEV(ms)(MonoS3.A, MonoS3.B, MonoS3.C)
      assertUnorderedNEV(cs)('a', 'b', 'c')
      for (m <- ms)
        assert(cm(mc(m)) == m)
    }

    "adtIsoSet" - {
      val (mc, cm, ms, cs) = adtIsoSet[MonoS3, Char] {
        case MonoS3.A => 'a'
        case MonoS3.B => 'b'
        case MonoS3.C => 'c'
      }
      assert(ms == NonEmptySet[MonoS3](MonoS3.A, MonoS3.B, MonoS3.C))
      assert(cs == NonEmptySet('a', 'b', 'c'))
      for (m <- ms)
        assert(cm(mc(m)) == m)
    }

  }
}
