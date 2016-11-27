package language.expressions

import bindingTypeMachine.{MachineClosureType, ExpressionScope, Machine, MachineType}
import constraints.objects.Reference
import constraints.scopes.objects.{ConcreteScope, Scope}
import constraints.types.{CheckSubType, TypesAreEqual}
import constraints.types.objects.{ConcreteType, ConstraintClosureType, ConstraintExpression, Type}
import constraints.{Constraint, ConstraintBuilder}
import language.Language
import language.types.LanguageType

class ContraVariantLambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val bodyScope: ConcreteScope = builder.newScope(Some(scope))
    val argumentType = builder.typeVariable()
    builder.declaration(name, this, bodyScope, Some(argumentType))
    val bodyType = builder.typeVariable()
    builder.typesAreEqual(_type, Language.getFunctionType(argumentType, bodyType))
    body.constraints(builder, bodyType, bodyScope)
    parameterDefinedType.foreach(at => {
      val parameterType = at.constraints(builder, scope)
      builder.add(CheckSubType(argumentType, parameterType))
    })
  }

  override def evaluate(machine: Machine): MachineType = {
    parameterDefinedType.foreach(t => {
      machine.enterScope()
      machine.declare(name, t.evaluate(machine))
      body.evaluate(machine)
      machine.exitScope()
    })
    MachineClosureType(machine.currentScope, name, (m: Machine) => {
      val actualArgumentType = m.resolve(name)
      parameterDefinedType.foreach(a => m.assertSubType(actualArgumentType, a.evaluate(machine)))
      body.evaluate(m)
    })
  }
}

class ClosureLambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {

  override def evaluate(machine: Machine): MachineType = new Lambda(name, body, parameterDefinedType).evaluate(machine)

  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val reference = Reference(name, this)
    val wrappedBody = parameterDefinedType.fold[ConstraintExpression](body)(t => new TypeCheckWrapper(reference, body, t.constraints(builder, parentScope)))
    builder.add(TypesAreEqual(_type, ConstraintClosureType(parentScope, reference, wrappedBody)))
  }

  class TypeCheckWrapper(name: Reference, original: ConstraintExpression, parameterType: Type) extends ConstraintExpression
  {
    override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
      val declaration = builder.declarationVariable(parameterType)
      builder.reference(name.name, name.id, parentScope, declaration)
      original.constraints(builder, _type, parentScope)
    }
  }
}

class Lambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val bodyScope: ConcreteScope = builder.newScope(Some(scope))
    val argumentConstraintType = builder.typeVariable()
    builder.declaration(name, this, bodyScope, Some(argumentConstraintType))
    val bodyType = builder.typeVariable()
    builder.typesAreEqual(_type, Language.getFunctionType(argumentConstraintType, bodyType))
    body.constraints(builder, bodyType, bodyScope)
    parameterDefinedType.foreach(at => at.constraints(builder, argumentConstraintType, scope))
  }

  override def evaluate(machine: Machine): MachineType = {
    parameterDefinedType.foreach(t => {
      machine.enterScope()
      machine.declare(name, t.evaluate(machine))
      body.evaluate(machine)
      machine.exitScope()
    })
    MachineClosureType(machine.currentScope, name, (m: Machine) => {
      val actualArgumentType = m.resolve(name)
      parameterDefinedType.foreach(a => m.assertEqual(a.evaluate(machine), actualArgumentType))
      body.evaluate(m)
    })
  }
}
