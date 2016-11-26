package language.types

import bindingTypeMachine.{Machine, MachineType, MachineTypeVariable, TypeCheckException}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.Type

case class LanguageTypeApplication(function: LanguageType, argument: LanguageType) extends LanguageType {
  override def variables: Set[LanguageTypeVariable] = function.variables ++ argument.variables

  override def evaluate(machine: Machine): MachineType = {
    val functionType = function.evaluate(machine)
    val functionVariables: Set[MachineTypeVariable] = functionType.variables
    if (functionVariables.size != 1)
      throw TypeCheckException(s"type application function type has ${functionVariables.size} variables")

    val variable = functionVariables.head
    functionType.instantiate(variable.name, argument.evaluate(machine))
  }

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = ???
}
