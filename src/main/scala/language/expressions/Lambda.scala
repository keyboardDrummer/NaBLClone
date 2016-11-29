package language.expressions

import bindingTypeMachine.{Machine, MachineClosureType, MachineType}
import constraints.ConstraintBuilder
import constraints.objects.NamedDeclaration
import constraints.scopes.objects.{ConcreteScope, Scope}
import constraints.types.CheckSubType
import constraints.types.objects.{ConstraintClosureType, ConstraintExpression, Type}
import language.types.LanguageType
import modes.{ConstraintClosure, ConstraintHindlerMilnerWithSubtyping, ConstraintHindleyMilner}

class ContraVariantLambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None)
  extends Lambda(name, body, parameterDefinedType) {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.mode match {
    case ConstraintHindleyMilner =>
      val bodyScope: ConcreteScope = builder.newScope(Some(scope))
      val argumentType = builder.declarationType(name, this, bodyScope)

      val bodyType = body.constraints(builder, bodyScope)
      builder.typesAreEqual(_type, builder.getFunctionType(argumentType, bodyType))

      parameterDefinedType.foreach(at => {
        val parameterType = at.constraints(builder, scope)
        builder.add(CheckSubType(argumentType, parameterType))
      })
    case _ => super.constraints(builder, _type, scope)
  }
}

case class Lambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = builder.mode match {
    case ConstraintHindlerMilnerWithSubtyping =>
      val bodyScope: ConcreteScope = builder.newScope(Some(scope))
      val argumentType = builder.declarationType(name, this, bodyScope)

      val bodyType = body.constraints(builder, bodyScope)
      builder.typesAreEqual(_type, builder.getFunctionType(argumentType, bodyType))

      parameterDefinedType.foreach(at => {
        val parameterType = at.constraints(builder, scope)
        builder.add(CheckSubType(argumentType, parameterType))
      })

    case ConstraintHindleyMilner =>
      val bodyScope: ConcreteScope = builder.newScope(Some(parentScope))
      val argumentConstraintType = builder.declarationType(name, this, bodyScope)

      val bodyType = body.constraints(builder, bodyScope)
      builder.typesAreEqual(_type, builder.getFunctionType(argumentConstraintType, bodyType))

      parameterDefinedType.foreach(at => at.constraints(builder, argumentConstraintType, parentScope))

    case ConstraintClosure =>
      val declaration = new NamedDeclaration(name, this)
      val wrappedBody = parameterDefinedType.fold[ConstraintExpression](body)(t => new TypeCheckWrapper(name, body, t.constraints(builder, parentScope)))
      builder.typesAreEqual(_type, ConstraintClosureType(parentScope, declaration, wrappedBody))
  }

  class TypeCheckWrapper(name: String, original: ConstraintExpression, parameterType: Type) extends ConstraintExpression
  {
    override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
      val declaration = builder.resolve(name, this, parentScope)
      builder.add(CheckSubType(builder.getType(declaration), parameterType))
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
      parameterDefinedType.foreach(a => m.assertSubType(actualArgumentType, a.evaluate(machine)))
      body.evaluate(m)
    })
  }
}
