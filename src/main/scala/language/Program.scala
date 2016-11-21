package language

import constraints.types.{AssignSubType, CheckSubType}
import constraints.{Constraint, ConstraintBuilder}
import language.modules.Module
import language.types.{IntType, LongType}

object Program
{

  def libraryConstraints: Seq[Constraint] = {
    Seq(AssignSubType(IntType, LongType))
  }
}

case class Program(modules: Seq[Module])
{
  def constraints(builder: ConstraintBuilder): Seq[Constraint] = {
    val scope = builder.newScope()
    Program.libraryConstraints ++ modules.flatten(module => module.constraints(builder, scope))
  }
}
