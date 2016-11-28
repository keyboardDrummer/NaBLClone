package language.modules

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import language.expressions.Expression
import language.types.LanguageType

class Binding(name: String, body: Expression, bindingType: Option[LanguageType] = None)
{
  def evaluate(machine: Machine): Unit = {
    val evaluatedType = body.evaluate(machine)
    val bindingMachineType = bindingType.fold(evaluatedType)(t => {
      val realType: MachineType = t.evaluate(machine)
      machine.assertEqual(realType, evaluatedType)
      realType
    })
    machine.currentModule.addBinding(name, bindingMachineType)
  }

  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit = {
    val typeVariable = bindingType.fold[Type](builder.typeVariable())(t => t.constraints(builder, parentScope))
    builder.declaration(name, this, parentScope, Some(typeVariable))
    body.constraints(builder, typeVariable, parentScope)
  }
}
