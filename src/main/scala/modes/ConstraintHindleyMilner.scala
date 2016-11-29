package modes

object ConstraintHindleyMilner
{
  def both: Set[Checker] = Set(ConstraintHindleyMilner(true), ConstraintHindleyMilner(false))
}

case class ConstraintHindleyMilner(subTyping: Boolean) extends ConstraintChecker
