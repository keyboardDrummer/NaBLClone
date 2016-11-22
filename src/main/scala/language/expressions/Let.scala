package language.expressions

import constraints.objects.NamedDeclaration
import constraints.scopes.objects.Scope
import constraints.scopes.{DeclarationInsideScope, ParentScope}
import constraints.types.{DeclarationOfType, Generalization}
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}

class Let(name: String, bindingValue: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val scope = builder.newScope(Some(parentScope))
    val bindingType = builder.typeVariable()
    val generalizedType = builder.typeVariable()
    builder.add(Generalization(generalizedType, bindingType))
    builder.declaration(name, this, scope, Some(generalizedType))
    bindingValue.constraints(builder, bindingType, parentScope)
    value.constraints(builder, _type, scope)
  }
}
