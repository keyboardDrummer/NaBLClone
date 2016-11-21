package language.expressions

import constraints.scopes.objects.{ConcreteScope, Scope}
import constraints.types.TypesAreEqual
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}
import language.Language
import language.types.LanguageType

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
