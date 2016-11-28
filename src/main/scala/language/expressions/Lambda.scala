package language.expressions

import bindingTypeMachine.{Machine, MachineClosureType, MachineType}
import constraints.ConstraintBuilder
import constraints.objects.{NamedDeclaration, Reference}
import constraints.scopes.objects.{ConcreteScope, Scope}
import constraints.types.objects.{ConstraintClosureType, ConstraintExpression, Type}
import constraints.types.{CheckSubType, TypesAreEqual}
import language.Language
import language.types.LanguageType
import modes.{ConstraintClosure, ConstraintHindleyMilner}

class ContraVariantLambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val bodyScope: ConcreteScope = builder.newScope(Some(scope))
    val argumentType = builder.declarationType(name, this, bodyScope)

    val bodyType = body.constraints(builder, bodyScope)
    builder.typesAreEqual(_type, builder.getFunctionType(argumentType, bodyType))

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

class Lambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = builder.mode match {
    case ConstraintHindleyMilner =>
      val bodyScope: ConcreteScope = builder.newScope(Some(parentScope))
      val argumentConstraintType = builder.declarationType(name, this, bodyScope)

      val bodyType = body.constraints(builder, bodyScope)
      builder.typesAreEqual(_type, builder.getFunctionType(argumentConstraintType, bodyType))

      parameterDefinedType.foreach(at => at.constraints(builder, argumentConstraintType, parentScope))

    case ConstraintClosure =>
      val declaration = NamedDeclaration(name, this)
      val wrappedBody = parameterDefinedType.fold[ConstraintExpression](body)(t => new TypeCheckWrapper(name, body, t.constraints(builder, parentScope)))
      builder.typesAreEqual(_type, ConstraintClosureType(parentScope, declaration, wrappedBody))
  }

  class TypeCheckWrapper(name: String, original: ConstraintExpression, parameterType: Type) extends ConstraintExpression
  {
    override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
      val declaration = builder.declarationVariable(parameterType)
      builder.reference(name, this, parentScope, declaration)
      original.constraints(builder, _type, parentScope)
    }
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
