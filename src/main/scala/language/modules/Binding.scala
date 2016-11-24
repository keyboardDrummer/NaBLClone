package language.modules

import bindingTypeMachine.Machine
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.ConcreteType
import language.expressions.Expression
import language.types.LanguageType

class Binding(name: String, _type: LanguageType, body: Expression)
{
  def evaluate(machine: Machine) = {
    val realType: ConcreteType = _type.evaluate(machine)
    machine.declare(name, this, realType)
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
