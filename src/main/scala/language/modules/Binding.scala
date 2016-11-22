package language.modules

import constraints.objects.NamedDeclaration
import constraints.scopes.DeclarationInsideScope
import constraints.scopes.objects.Scope
import constraints.types.DeclarationOfType
import constraints.{Constraint, ConstraintBuilder}
import language.expressions.Expression
import language.types.LanguageType

class Binding(name: String, _type: LanguageType, body: Expression)
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope) = {
    val typeVariable = builder.typeVariable()
    builder.declaration(name, this, parentScope, Some(typeVariable))
    body.constraints(builder, typeVariable, parentScope)
    _type.constraints(builder, typeVariable, parentScope)
  }
}
