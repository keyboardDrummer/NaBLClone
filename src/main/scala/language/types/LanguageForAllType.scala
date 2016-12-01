package language.types

import bindingTypeMachine.{Machine, MachineType, MachineTypeVariable}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.Type

case class LanguageForAllType(variable: String, body: LanguageType) extends LanguageType {
  override def evaluate(machine: Machine): MachineType = {
    machine.enterTypeScope()
    machine.declareType(variable, MachineTypeVariable(variable))
    val result = body.evaluate(machine)
    machine.exitScope()
    result
  }

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
//    if (body.variables != Set(LanguageTypeVariable(variable)))
//    {
//      throw TypeCheckException("language forall variables don't add up") //TODO gekke check tijdens constraint collectie
//    }
    body.constraints(builder, _type, scope)
  }

  override def variables: Set[LanguageTypeVariable] = body.variables.diff(Set(LanguageTypeVariable(variable)))
}
