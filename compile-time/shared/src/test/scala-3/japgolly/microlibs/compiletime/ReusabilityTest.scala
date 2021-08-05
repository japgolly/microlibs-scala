package japgolly.microlibs.compiletime

import utest._

object ReusabilityTest extends TestSuite:

  object Recursive:

    sealed trait Item[+A]

    object Item:
      final case class Folder[+Y](name: String, indirect: Vector[Item[Y]], direct: Item[Y]) extends Item[Y]
      final case class Suite[+Z](bms: Vector[BM[Z]]) extends Item[Z]
      final case class Blah1[+B](blah: Option[Blah1[B]]) extends Item[B]
      final case class Blah2[+C](blah: Option[Blah2[C]], i: Int) extends Item[C]
      final case class Blah3(i: Int) extends Item[Nothing]

      final case class BM[+W](value: W)

      implicit def reusabilityB[A: Reusability]: Reusability[BM    [A]] = Reusability.derive
      implicit def reusabilityS[A: Reusability]: Reusability[Suite [A]] = Reusability.derive
      implicit def reusabilityF[A: Reusability]: Reusability[Folder[A]] = Reusability.derive
      implicit def reusability [A: Reusability]: Reusability[Item  [A]] = Reusability.derive

      // implicit def reusability [A: Reusability]: Reusability[Item  [A]] = {
      //   var a1: Reusability[Item.Blah1[A]] = new Reusability[Item.Blah1[A]](null)
      //   var a2: Reusability[Item.Blah2[A]] = new Reusability[Item.Blah2[A]](null)
      //   var a3: Reusability[Item.Blah3] = new Reusability[Item.Blah3](null)
      //   var a4: Reusability[Item[A]] = new Reusability[Item[A]](null)
      //   implicit lazy val _a1: Reusability[Item.Blah1[A]] = Reusability.suspend[Item.Blah1[A]](a1)
      //   implicit lazy val _a2: Reusability[Item.Blah2[A]] = Reusability.suspend[Item.Blah2[A]](a2)
      //   implicit lazy val _a3: Reusability[Item.Blah3] = Reusability.suspend[Item.Blah3](a3)
      //   implicit lazy val _a4: Reusability[Item[A]] = Reusability.suspend[Item[A]](a4)
      //   lazy val b1: Reusability[Item.Folder[A]] = MacroEnvStatic.inlineSummon[Reusability[Item.Folder[A]]]
      //   lazy val b2: Reusability[Item.Suite[A]] = MacroEnvStatic.inlineSummon[Reusability[Item.Suite[A]]]
      //   lazy val b3: Reusability[Option[Item.Blah2[A]]] = MacroEnvStatic.inlineSummon[Reusability[Option[Item.Blah2[A]]]]
      //   lazy val b4: Reusability[Int] = MacroEnvStatic.inlineSummon[Reusability[Int]]
      //   a1 = Reusability.by[Item.Blah1[A], Option[Item.Blah1[A]]](((a: Item.Blah1[A]) => a.asInstanceOf[Product].productElement(0).asInstanceOf[Option[Item.Blah1[A]]]))(MacroEnvStatic.inlineSummon[Reusability[Option[Item.Blah1[A]]]])
      //   a2 = new Reusability[Item.Blah2[A]](((`a₂`: Item.Blah2[A], b: Item.Blah2[A]) => b3.test(`a₂`.asInstanceOf[Product].productElement(0).asInstanceOf[Option[Item.Blah2[A]]], b.asInstanceOf[Product].productElement(0).asInstanceOf[Option[Item.Blah2[A]]]).&&(b4.test(`a₂`.asInstanceOf[Product].productElement(1).asInstanceOf[Int], b.asInstanceOf[Product].productElement(1).asInstanceOf[Int]))))
      //   a3 = Reusability.by[Item.Blah3, Int](((`a₃`: Item.Blah3) => `a₃`.asInstanceOf[Product].productElement(0).asInstanceOf[Int]))(MacroEnvStatic.inlineSummon[Reusability[Int]])
      //   a4 = {
      //     val m: deriving.Mirror.SumOf[Item[A]] = Item.$asInstanceOf$[deriving.Mirror {
      //       type MirroredType >: Item[A] <: Item[A]
      //       type MirroredMonoType >: Item[A] <: Item[A]
      //       type MirroredElemTypes >: Nothing <: Tuple
      //     } & deriving.Mirror.Sum {
      //       type MirroredMonoType >: Item[A] <: Item[A]
      //       type MirroredType >: Item[A] <: Item[A]
      //       type MirroredLabel >: "Item" <: "Item"
      //     } {
      //       type MirroredElemTypes >: *:[Item.Folder[A], *:[Item.Suite[A], *:[Item.Blah1[A], *:[Item.Blah2[A], *:[Item.Blah3, Tuple$package.EmptyTuple]]]]] <: *:[Item.Folder[A], *:[Item.Suite[A], *:[Item.Blah1[A], *:[Item.Blah2[A], *:[Item.Blah3, Tuple$package.EmptyTuple]]]]]
      //       type MirroredElemLabels >: *:["Folder", *:["Suite", *:["Blah1", *:["Blah2", *:["Blah3", Tuple$package.EmptyTuple]]]]] <: *:["Folder", *:["Suite", *:["Blah1", *:["Blah2", *:["Blah3", Tuple$package.EmptyTuple]]]]]
      //     }]
      //     val tests: collection.immutable.Vector[Function2[Item[A], Item[A], Boolean]] = Vector.apply[Function2[Item[A], Item[A], Boolean]](((`a₄`: Item[A], `b₂`: Item[A]) => b1.test(`a₄`.asInstanceOf[Item.Folder[A]], `b₂`.asInstanceOf[Item.Folder[A]])), ((`a₅`: Item[A], `b₃`: Item[A]) => b2.test(`a₅`.asInstanceOf[Item.Suite[A]], `b₃`.asInstanceOf[Item.Suite[A]])), ((`a₆`: Item[A], `b₄`: Item[A]) => a1.test(`a₆`.asInstanceOf[Item.Blah1[A]], `b₄`.asInstanceOf[Item.Blah1[A]])), ((`a₇`: Item[A], `b₅`: Item[A]) => a2.test(`a₇`.asInstanceOf[Item.Blah2[A]], `b₅`.asInstanceOf[Item.Blah2[A]])), ((`a₈`: Item[A], `b₆`: Item[A]) => a3.test(`a₈`.asInstanceOf[Item.Blah3], `b₆`.asInstanceOf[Item.Blah3])))
      //     new Reusability[Item[A]](((`a₉`: Item[A], `b₇`: Item[A]) => {
      //       val o: Int = m.ordinal(`a₉`)
      //       o.==(m.ordinal(`b₇`)).&&(tests(o)(`a₉`, `b₇`))
      //     }))
      //   }
      //   a4
      // }
  end Recursive

  case class C0()
  case class C1(x: Int)
  case class C2(x: Int, y: Int)
  implicit val c0: Reusability[C0] = Reusability.derive[C0]
  implicit val c1: Reusability[C1] = Reusability.derive[C1]
  implicit val c2: Reusability[C2] = Reusability.derive[C2]

  def test[A](a: A, b: A, expect: Boolean)(implicit r: Reusability[A]) =
    assert(r.test(a, b) == expect)

  override def tests = Tests {
    "0" - test(C0(), C0(), true)
    "1a" - test(C1(1), C1(1), true)
    "1b" - test(C1(1), C1(2), false)
    "2a" - test(C2(1,1), C2(1,1), true)
    "2b" - test(C2(1,1), C2(2,1), false)
    "2c" - test(C2(1,1), C2(1,2), false)

    "recursive" - {
      import Recursive._, Item._
      def test(a: Item[Int], b: Item[Int]): Unit =
        ReusabilityTest.test(a, b, expect = a == b)

      val values = List[Item[Int]](
        Blah1(None),
        Blah2(None, 2),
        Blah2(None, 1),
        Blah2(Some(Blah2(None, 1)), 1),
        Blah2(Some(Blah2(None, 2)), 1),
        Blah2(Some(Blah2(None, 1)), 2),
        Suite(Vector.empty),
        Suite(Vector(BM(1))),
        Suite(Vector(BM(1), BM(2))),
        Suite(Vector(BM(2), BM(1))),
        Folder("hehe", Vector.empty, Blah1(None)),
        Folder("hehe", Vector.empty, Blah2(None, 1)),
        Folder("he!he", Vector.empty, Blah1(None)),
        Folder("hehe", Vector(Blah1(None)), Blah1(None)),
        Folder("hehe", Vector(Blah1(None), Blah1(None)), Blah1(None)),
      )

      for {
        a <- values
        b <- values
      } test(a, b)
    }

  }
