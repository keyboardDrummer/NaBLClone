package language.expressions

import constraints.scopes.objects.{ConcreteScope, Scope}
import constraints.types.{CheckSubType, TypesAreEqual}
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}
import language.Language
import language.types.LanguageType

class ContraVariantLambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val bodyScope: ConcreteScope = builder.newScope(Some(scope))
    val argumentType = builder.typeVariable()
    val parameterType = builder.typeVariable()
    builder.declaration(name, this, bodyScope, Some(parameterType))
    val bodyType = builder.typeVariable()
    builder.getConstraints ++ Seq(CheckSubType(argumentType, parameterType), TypesAreEqual(_type, Language.getFunctionType(argumentType, bodyType))) ++
      body.constraints(builder, bodyType, bodyScope) ++
      parameterDefinedType.fold(Seq.empty[Constraint])(at => at.constraints(builder, parameterType, scope))
  }
}

class Lambda(name: String, body: Expression, argumentType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val bodyScope: ConcreteScope = builder.newScope(Some(scope))
    val argumentConstraintType = builder.typeVariable()
    builder.declaration(name, this, bodyScope, Some(argumentConstraintType))
    val bodyType = builder.typeVariable()
    builder.getConstraints ++ Seq(TypesAreEqual(_type, Language.getFunctionType(argumentConstraintType, bodyType))) ++
        body.constraints(builder, bodyType, bodyScope) ++
        argumentType.fold(Seq.empty[Constraint])(at => at.constraints(builder, argumentConstraintType, scope))
  }
}
