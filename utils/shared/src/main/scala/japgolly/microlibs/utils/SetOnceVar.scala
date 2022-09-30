package japgolly.microlibs.utils

final class SetOnceVar[A] {
  private var instance = Option.empty[A]

  def getOrSet(value: => A): A =
    synchronized {
      if (instance.isDefined)
        instance.get
      else {
        val a = value
        instance = Some(a)
        a
      }
    }

  def getOption(): Option[A] =
    synchronized(instance)

  def getOrThrow(): A =
    getOrThrow("SetOnceVar not yet set.")

  def getOrThrow(errMsg: => String): A =
    getOption().getOrElse(throw new RuntimeException(errMsg))
}

object SetOnceVar {
  def apply[A]: SetOnceVar[A] =
    new SetOnceVar
}
