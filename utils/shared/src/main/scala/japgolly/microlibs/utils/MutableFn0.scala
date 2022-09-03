package japgolly.microlibs.utils

import scala.runtime.AbstractFunction0

trait MutableFn0[A] extends AbstractFunction0[A] {
  def getFn: () => A
  def setFn(f: () => A): this.type

  final def set(f: => A): this.type =
    setFn(() => f)
}

object MutableFn0 {

  def fromVariable[A](v: Variable[() => A]): MutableFn0[A] =
    new MutableFn0[A] {

      override def apply(): A =
        v.get()()

      override def getFn: () => A =
        v.get()

      override def setFn(f: () => A): this.type = {
        v.set(f)
        this
      }
    }

  class Dsl[A](make: (() => A) => Variable[() => A]) {

    def apply(a: => A): MutableFn0[A] =
      withFn(() => a)

    def withFn(f: () => A): MutableFn0[A] =
      MutableFn0.fromVariable(make(f))
  }

  class DslWithDefault[A](default: () => A, make: (() => A) => Variable[() => A]) extends Dsl(make) {
    def apply(): MutableFn0[A] =
      withFn(default)
  }

  class DslWithDefaultSelector[A](default: () => A) {
    def threadUnsafe = new DslWithDefault[A](default, Variable(_))
    def volatile = new DslWithDefault[A](default, Variable.volatile(_))
    def syncOn(lock: AnyRef) = new DslWithDefault[A](default, Variable.syncOn(lock))
  }

  // ===================================================================================================================

  def withDefault[A](a: => A): DslWithDefaultSelector[A] =
    new DslWithDefaultSelector[A](() => a)

  def threadUnsafe[A] = new Dsl[A](Variable(_))
  def volatile[A] = new Dsl[A](Variable.volatile(_))
  def syncOn[A](lock: AnyRef) = new Dsl[A](Variable.syncOn(lock))
}
