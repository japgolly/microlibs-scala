package japgolly.microlibs.testutil

import scala.annotation.nowarn

object TypeTestingUtil extends TypeTestingUtil

trait TypeTestingUtil {

  def assertType[A]: TypeTestingUtilDsl[A] =
    new TypeTestingUtilDsl[A]

  @nowarn("cat=unused")
  def assertTypeOf[A](a: => A): TypeTestingUtilDsl[A] =
    assertType[A]

  @nowarn("cat=unused")
  def assertTypeOfImplicit[A](implicit a: A): TypeTestingUtilDsl[a.type] =
    assertType[a.type]
}

@nowarn("cat=unused")
class TypeTestingUtilDsl[A] {
  def map[B](f: A => B): TypeTestingUtilDsl[B] =
    TypeTestingUtil.assertType[B]

  def is          [B](implicit ev: A =:= B): Unit = ()
  def is_<        [B](implicit ev: A <:< B): Unit = ()
  def is_>        [B](implicit ev: B <:< A): Unit = ()
  def isImplicitly[B](implicit ev: A => B) : Unit = ()
}
