package language.modules

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import language.expressions.Expression
import language.types.LanguageType

class Binding(name: String, _type: LanguageType, body: Expression)
{
  def evaluate(machine: Machine) = {
    val realType: MachineType = _type.evaluate(machine)
    machine.currentModule.addBinding(name, realType)
    val evaluatedType = body.evaluate(machine)
    machine.assertEqual(realType, evaluatedType)
  }

  def constraints(builder: ConstraintBuilder, parentScope: Scope) = {
    val typeVariable = builder.typeVariable()
    builder.declaration(name, this, parentScope, Some(typeVariable))
    body.constraints(builder, typeVariable, parentScope)
    _type.constraints(builder, typeVariable, parentScope)
  }
}
