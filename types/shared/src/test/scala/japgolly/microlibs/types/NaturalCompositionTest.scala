package japgolly.microlibs.types

import japgolly.microlibs.testutil.TestUtil._

object NaturalCompositionTest {
  import NaturalComposition.{Merge, Split}

  def assertSplit[A, B](implicit m: Split[A, B]) = assertType[m.In]
  def assertMerge[A, B](implicit m: Merge[A, B]) = assertType[m.Out]

  type U = Unit
  type I = Int
  type S = String

  assertSplit[I, U].is[I]
  assertSplit[I, I].is[I] // different
  assertSplit[I, S].is[(I, S)]
  assertSplit[3, 3].is[3]
  assertSplit[3, 4].is[(3, 4)]
  assertSplit[3, I].is[(3, I)]
  assertSplit[U, I].is[I]
  assertSplit[U, U].is[U]

  assertMerge[I, U].is[I]
  assertMerge[I, I].is[(I, I)] // different
  assertMerge[I, S].is[(I, S)]
  assertMerge[3, 3].is[3]
  assertMerge[3, 4].is[(3, 4)]
  assertMerge[3, I].is[(3, I)]
  assertMerge[U, I].is[I]
  assertMerge[U, U].is[U]
}
