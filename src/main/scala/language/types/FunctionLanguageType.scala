package language.types

import bindingTypeMachine.{FunctionType, Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import language.Language

case class FunctionLanguageType(argument: LanguageType, result: LanguageType) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope) = {
    val inputType = builder.typeVariable()
    val outputType = builder.typeVariable()
    builder.typesAreEqual(_type, Language.getFunctionType(inputType, outputType))
    argument.constraints(builder, inputType, scope)
    result.constraints(builder, outputType, scope)
  }

  override def evaluate(machine: Machine): MachineType = FunctionType(argument.evaluate(machine), result.evaluate(machine))

  override def variables: Set[LanguageTypeVariable] = argument.variables ++ result.variables
}
