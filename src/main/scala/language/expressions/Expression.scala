package language.expressions

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.{ConstraintExpression, Type}
import language.structs.{Access, StructFieldInit}

trait Expression extends ConstraintExpression {
  def evaluate(machine: Machine): MachineType

  def apply(argument: Expression) = Application(this, argument)

  def access(field: String) = Access(this, field)

}
