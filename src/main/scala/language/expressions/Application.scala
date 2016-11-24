package language.expressions

import bindingTypeMachine.Machine
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

  override def evaluate(machine: Machine): ConcreteType = ???
}

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val functionType = builder.typeVariable()
    val argumentType = builder.typeVariable()
    function.constraints(builder, functionType, scope)
    builder.add(Seq(TypesAreEqual(functionType, Language.getFunctionType(argumentType, _type))))
    value.constraints(builder, argumentType, scope)
  }

  override def evaluate(machine: Machine): ConcreteType = {
    val functionType = function.evaluate(machine)
    functionType match
    {
      case AppliedType("Func", Seq(input: ConcreteType, output: ConcreteType)) =>
        val argumentType = value.evaluate(machine)
        machine.assertEqual(input, argumentType)
        output
      case _ => throw new IllegalStateException()
    }
  }
}
