package japgolly.microlibs.multimap

import japgolly.microlibs.multimap.MultiValues.Commutative
import japgolly.microlibs.multimap.Multimap.Internal._
import org.scalacheck._
import org.scalacheck.Prop._
import scala.collection.immutable.ArraySeq
import utest._

object MultimapTest extends TestSuite {

  private def propEq[R](name: String, actual: => R, expect: => R): Prop =
    (actual ?= expect) :| name

  case class PropInputs[L[_]: MultiValues, A](
    mm: Multimap[A, L, A],
    a: A,
    b: A,
    as: L[A],
    commutative: Option[Commutative[L]]
  ) {

    val em = Multimap.empty[A, L, A]
    val el = MultiValues[L].empty[A]

    def l(x: A): L[A] = el.add1(x)

    lazy val ab    = l(a).add1(b)
    lazy val bigas = as.add1(a).add1(b)
    lazy val bigm  = mm.addks(bigas, a).addvs(b, bigas)

    type MM = Multimap[A, L, A]
    def comm[R](name: => String, z: MM, r: MM => R, f: MM => MM, g: MM => MM): Prop =
      propEq(name, r(f(g(z))), r(g(f(z))))

    def commutativeProps: Seq[Prop] =
      commutative.fold(Seq.empty[Prop]) { c =>
        implicit val cc: Commutative[L] = c
        Seq(
          propEq("reverse.reverse = id"      , mm.reverse.reverse                     , mm),
          propEq("addvs symmetrical to addks", mm.addvs(a, as).reverse                , mm.reverse.addks(as, a)),
          propEq("delvs symmetrical to delks", bigm.delvs(ab).reverse                 , bigm.reverse.delks(ab)),
          propEq("delv symmetrical to delk",   bigm.delv(b).reverse                   , bigm.reverse.delk(b)),
          propEq("get⁻¹.setks.add = ks",       em.add(b, a).setks(as, a).reverse.apply(a), as)
        )
      }

    def generalProps: Seq[Prop] =
      Seq(
        propEq("get.setvs.add = vs",         em.add(a, b).setvs(a, as)(a), as),
        propEq("get.delk.add = ∅",           em.add(a, b).delk(a)(a)     , el),
        propEq("get.add.delk = v",           em.delk(a).add(a, b)(a)     , l(b)),
        propEq("get.delk.addvs = ∅",         em.addvs(a, as).delk(a)(a)  , el),
        propEq("get.addvs.delk = vs",        em.delk(a).addvs(a, as)(a)  , as),
        ((a != b) ==> (em.addvs(a, as).delk(b)(a) ?= as)) :| "get₁.delk₂.addvs₁ = vs",
        propEq("delk.delk = delks",          bigm.delk(a).delk(b)        , bigm.delks(ab)),
        propEq("delkv == delv.delk",         bigm.delk(a).delv(a)        , bigm.delkv(a)),
        propEq("unlink == del(kv).del(vk)",  bigm.del(a, b).del(b, a)    , bigm.unlink(a, b)),
        comm("addvs.add ⊇⊆ add.addvs", em, _(a).set, _.addvs(a, as), _.add(a, b)),
        comm("addvs₁₂ ⊇⊆ addvs₂₁",     em, _(a).set, _.addvs(a, as), _.addvs(a, l(b)))
      )

    def eval: Prop =
      Prop.all(commutativeProps ++ generalProps: _*)
  }

  implicit def commutativeO[L[_]](implicit c: Commutative[L] = null): Option[Commutative[L]] = Option(c)

  def gen[L[_]: MultiValues, A](ga: Gen[A], gl: Gen[A] => Gen[L[A]])
                               (implicit c: Option[Commutative[L]]): Gen[PropInputs[L, A]] = {
    val gla = gl(ga)
    for {
      kvs <- Gen.listOf(for { k <- ga; vs <- gla } yield (k, vs))
      mm   = Multimap(kvs.toMap)
      a   <- ga
      b   <- ga
      as  <- gla
    } yield PropInputs[L, A](mm, a, b, as, c)
  }

  private val genList    : Gen[PropInputs[List,     Int ]] = gen(Gen.choose(-100, 100), Gen.listOf(_))
  private val genSet     : Gen[PropInputs[Set,      Int ]] = gen(Gen.choose(-100, 100), Gen.listOf(_).map(_.toSet))
  private val genVector  : Gen[PropInputs[Vector,   Long]] = gen(Gen.choose(-100L, 100L), Gen.listOf(_).map(_.toVector))
  private val genArraySeq: Gen[PropInputs[ArraySeq, Long]] = gen(Gen.choose(-100L, 100L), Gen.listOf(_).map(ArraySeq.from))

  private def checkProp(p: Prop): Unit = {
    val result = Test.check(Test.Parameters.default, p)
    if (!result.passed) {
      val msg = result.status match {
        case Test.Failed(args, labels) =>
          s"Property test failed with args: $args, labels: $labels"
        case Test.PropException(args, e, labels) =>
          s"Property test threw exception with args: $args, labels: $labels, exception: $e"
        case other =>
          s"Property test failed with status: $other"
      }
      throw new java.lang.AssertionError(msg)
    }
  }

  override def tests = Tests {

    "prop" - {
      "list"     - checkProp(Prop.forAll(genList)(_.eval))
      "set"      - checkProp(Prop.forAll(genSet)(_.eval))
      "vector"   - checkProp(Prop.forAll(genVector)(_.eval))
      "arraySeq" - checkProp(Prop.forAll(genArraySeq)(_.eval))
    }

    "delv" - {
      "removesEmptyKey" - {
        val mm = Multimap(Map(1 -> Set("a"), 2 -> Set("a", "b")))
        val mm2 = mm.delv("a")
        assert(mm2.keyCount == 1)
        assert(mm2.keys.toList == List(2))
        assert(!mm2.m.contains(1))
        assert(mm2(1).isEmpty)
      }

      "emptyResult" - {
        val mm = Multimap(Map(1 -> Set("a")))
        val mm2 = mm.delv("a")
        assert(mm2.isEmpty)
        assert(!mm2.nonEmpty)
        assert(mm2.keyCount == 0)
        assert(mm2.keys.isEmpty)
      }
    }

    "delvs" - {
      "removesEmptyKey" - {
        val mm = Multimap(Map(1 -> Set("a", "b"), 2 -> Set("a", "b", "c")))
        val mm2 = mm.delvs(Set("a", "b"))
        assert(mm2.keyCount == 1)
        assert(mm2.keys.toList == List(2))
        assert(!mm2.m.contains(1))
        assert(mm2(1).isEmpty)
      }

      "emptyResult" - {
        val mm = Multimap(Map(1 -> Set("a", "b")))
        val mm2 = mm.delvs(Set("a", "b"))
        assert(mm2.isEmpty)
        assert(!mm2.nonEmpty)
        assert(mm2.keyCount == 0)
        assert(mm2.keys.isEmpty)
      }
    }
  }
}
