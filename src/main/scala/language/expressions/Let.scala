package language.expressions

import constraints.objects.NamedDeclaration
import constraints.scopes.objects.Scope
import constraints.scopes.{DeclarationInsideScope, ParentScope}
import constraints.types.DeclarationOfType
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}

class Let(name: String, bindingValue: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val scope = builder.newScope(Some(parentScope))
    val bindingType = builder.typeVariable()
    builder.declaration(name, this, scope, Some(bindingType))
    bindingValue.constraints(builder, bindingType, parentScope)
    value.constraints(builder, _type, scope)
  }
}
