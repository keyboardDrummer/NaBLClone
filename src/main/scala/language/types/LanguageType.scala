package language.types

import bindingTypeMachine.Machine
import constraints.scopes.objects.Scope
import constraints.types.objects.{ConcreteType, Type}
import constraints.{Constraint, ConstraintBuilder}

trait LanguageType
{
  def evaluate(machine: Machine): ConcreteType = ???

  def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope)
}
