package language.expressions

import bindingTypeMachine.Machine
import constraints.scopes.objects.Scope
import constraints.types.objects.{ConcreteType, Type}
import constraints.{Constraint, ConstraintBuilder}

trait Expression {
  def evaluate(machine: Machine): ConcreteType

  def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit
}
