package japgolly.microlibs.utils

trait Variable[@specialized A] { self =>
  def get(): A
  def mod(f: A => A): A

  def set(a: A): A =
    mod(_ => a)

  def xmap[B](f: A => B)(g: B => A): Variable[B] =
    new Variable[B] {
      override def get() = f(self.get())
      override def mod(h: B => B) = f(self.mod(a => g(h(f(a)))))
    }
}

object Variable {

  def apply[@specialized A](init: A): Variable[A] =
    new Variable[A] {
      private[this] var value = init
      override def get() = value
      override def mod(f: A => A) = { value = f(value); value }
    }

  def volatile[@specialized A](init: A): Variable[A] =
    new Variable[A] {
      @volatile private[this] var value = init
      override def get() = value
      override def mod(f: A => A) = { value = f(value); value }
    }

  def syncOn[@specialized A](lock: AnyRef)(init: A): Variable[A] =
    new Variable[A] {
      private[this] var value = init
      override def get() = lock.synchronized(value)
      override def mod(f: A => A) = lock.synchronized { value = f(value); value }
    }
}
