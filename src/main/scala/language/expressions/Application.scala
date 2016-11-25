package language.expressions

import bindingTypeMachine.{ClosureType, Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.{AppliedType, ConcreteType, Type}
import constraints.types.{CheckSubType, TypesAreEqual}
import language.Language

case class ContraVariantApplication(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val functionType = builder.typeVariable()
    val argumentType = builder.typeVariable()
    val parameterType = builder.typeVariable()
    builder.add(Seq(TypesAreEqual(functionType, Language.getFunctionType(parameterType, _type)), CheckSubType(argumentType, parameterType)) )
    function.constraints(builder, functionType, scope)
    value.constraints(builder, argumentType, scope)
  }

  override def evaluate(machine: Machine): MachineType = Application(function, value).evaluate(machine)
}

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val functionType = builder.typeVariable()
    val argumentType = builder.typeVariable()
    function.constraints(builder, functionType, scope)
    builder.add(Seq(TypesAreEqual(functionType, Language.getFunctionType(argumentType, _type))))
    value.constraints(builder, argumentType, scope)
  }

  override def evaluate(machine: Machine): MachineType = {
    val functionType = function.evaluate(machine)
    val argumentType = value.evaluate(machine)
    functionType match
    {
      case ClosureType(environment, name, getType) =>
        val currentScope = machine.currentScope
        machine.currentScope = environment
        machine.enterScope()
        machine.declare(name, argumentType)
        val result = getType(machine)
        machine.exitScope()
        machine.currentScope = currentScope
        result
      case _ => throw new IllegalStateException()
    }
  }
}
