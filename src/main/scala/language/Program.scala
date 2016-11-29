package language

import bindingTypeMachine.{IntMachineType, LongMachineType, Machine}
import constraints.types.objects.{IntConstraintType, LongConstraintType}
import constraints.types.{AssignSubType, CheckSubType}
import constraints.{Constraint, ConstraintBuilder}
import language.modules.Module

object Program
{
  def libraryConstraints: List[Constraint] = {
    List(AssignSubType(IntConstraintType, LongConstraintType))
  }
}

case class Program(modules: Seq[Module])
{
  def constraints(builder: ConstraintBuilder): Unit = {
    builder.add(Program.libraryConstraints)
    val scope = builder.newScope()
    modules.foreach(module => module.constraints(builder, scope))
  }

  def evaluate(machine: Machine): Unit =
  {
    machine.addSubType(IntMachineType, LongMachineType)
    modules.foreach(module => module.bind(machine))
    modules.foreach(module => module.evaluate(machine))
  }
}
