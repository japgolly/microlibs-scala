package japgolly.microlibs.utils

sealed trait Validity extends SafeBool[Validity] with SafeBool.WithBoolOps[Validity] {
  override final def companion = Validity
}

object Validity extends SafeBool.Object[Validity] {
  override def positive = Valid
  override def negative = Invalid

  def apply(d: Either[Any, Any]): Validity =
    Valid when d.isRight
}

case object Valid extends Validity {
  val always: Any => Validity = _ => Valid
}

case object Invalid extends Validity
