package language.types

import bindingTypeMachine.{Machine, MachineType, MachineTypeVariable, TypeCheckException}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.Type

trait LanguageType
{
  def variables: Set[LanguageTypeVariable]

  def evaluate(machine: Machine): MachineType

  def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope)
}

case class LanguageTypeVariable(name: String) extends LanguageType {
  override def evaluate(machine: Machine): MachineType = MachineTypeVariable(name)

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = ???

  override def variables: Set[LanguageTypeVariable] = Set(this)
}

case class LanguageForAllType(variable: String, body: LanguageType) extends LanguageType {
  override def evaluate(machine: Machine): MachineType = {
    if (body.variables != Set(LanguageTypeVariable(variable)))
    {
      throw TypeCheckException("language forall variables don't add up")
    }
    body.evaluate(machine)
  }

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = ???

  override def variables: Set[LanguageTypeVariable] = body.variables.diff(Set(LanguageTypeVariable(variable)))
}
