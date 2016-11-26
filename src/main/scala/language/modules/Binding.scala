package language.modules

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import language.expressions.Expression
import language.types.LanguageType

class Binding(name: String, body: Expression, _type: Option[LanguageType] = None)
{
  def evaluate(machine: Machine) = {
    val evaluatedType = body.evaluate(machine)
    val bindingType = _type.fold(evaluatedType)(t => {
      val realType: MachineType = t.evaluate(machine)
      machine.assertEqual(realType, evaluatedType)
      realType
    })
    machine.currentModule.addBinding(name, bindingType)
  }

  def constraints(builder: ConstraintBuilder, parentScope: Scope) = {
    val typeVariable = _type.fold[Type](builder.typeVariable())(t => t.constraints(builder, parentScope))
    builder.declaration(name, this, parentScope, Some(typeVariable))
    body.constraints(builder, typeVariable, parentScope)
  }
}
