package language.expressions

import constraints.scopes.objects.{ConcreteScope, Scope}
import constraints.types.{CheckSubType, TypesAreEqual}
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}
import language.Language
import language.types.LanguageType

class ContraVariantLambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val bodyScope: ConcreteScope = builder.newScope(Some(scope))
    val argumentType = builder.typeVariable()
    val parameterType = builder.typeVariable()
    builder.declaration(name, this, bodyScope, Some(parameterType))
    val bodyType = builder.typeVariable()
    builder.add(Seq(CheckSubType(argumentType, parameterType), TypesAreEqual(_type, Language.getFunctionType(argumentType, bodyType))))
    body.constraints(builder, bodyType, bodyScope)
    parameterDefinedType.foreach(at => at.constraints(builder, parameterType, scope))
  }
}

class Lambda(name: String, body: Expression, argumentType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val bodyScope: ConcreteScope = builder.newScope(Some(scope))
    val argumentConstraintType = builder.typeVariable()
    builder.declaration(name, this, bodyScope, Some(argumentConstraintType))
    val bodyType = builder.typeVariable()
    builder.typesAreEqual(_type, Language.getFunctionType(argumentConstraintType, bodyType))
    body.constraints(builder, bodyType, bodyScope)
    argumentType.foreach(at => at.constraints(builder, argumentConstraintType, scope))
  }
}
