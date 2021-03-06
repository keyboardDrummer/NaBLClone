package language.expressions

import bindingTypeMachine.{Machine, MachineClosureType, MachineType, _}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.Type

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val functionType = function.getType(builder, scope)
    val argumentType = value.getType(builder, scope)
    builder.typesAreEqual(functionType, builder.getFunctionType(argumentType, _type, this))
  }

  override def evaluate(machine: Machine): MachineType = {
    val functionType = function.evaluate(machine)
    val argumentType = value.evaluate(machine)
    functionType match
    {
      case MachineClosureType(environment, name, getType) =>
        val currentScope = machine.currentScope
        machine.currentScope = environment
        machine.enterScope()
        machine.declare(name, argumentType)
        val result = getType(machine)
        machine.exitScope()
        machine.currentScope = currentScope
        result

      case FunctionType(input, output) =>
        machine.assertEqual(input, argumentType)
        output

      case _ =>
        throw TypeCheckException(s"cannot apply $argumentType to non function type $functionType")
    }
  }
}
