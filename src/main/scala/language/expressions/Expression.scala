package language.expressions

import bindingTypeMachine.{Machine, MachineType}
import constraints.scopes.objects.Scope
import constraints.types.objects.{ConcreteType, Type}
import constraints.{Constraint, ConstraintBuilder}
import language.structs.Access

trait Expression {
  def evaluate(machine: Machine): MachineType

  def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit

  def apply(argument: Expression) = Application(this, argument)
  def $: (Expression) => Application = apply

  def access(field: String) = new Access(this, field)
}
