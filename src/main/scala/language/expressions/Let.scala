package language.expressions

import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.Generalization
import constraints.types.objects.Type

class NoGeneralizeLet(name: String, bindingValue: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val scope = builder.newScope(Some(parentScope))
    val bindingType = builder.typeVariable()
    builder.declaration(name, this, scope, Some(bindingType))
    bindingValue.constraints(builder, bindingType, parentScope)
    value.constraints(builder, _type, scope)
  }
}

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
