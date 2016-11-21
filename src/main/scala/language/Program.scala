package language

import constraints.{Constraint, ConstraintBuilder}
import language.modules.Module

case class Program(modules: Seq[Module])
{
  def constraints(builder: ConstraintBuilder): Seq[Constraint] = {
    val scope = builder.newScope()
    modules.flatten(module => module.constraints(builder, scope))
  }
}
