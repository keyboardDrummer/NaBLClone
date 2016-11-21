package language.expressions

import constraints.objects.NamedDeclaration
import constraints.scopes.objects.Scope
import constraints.scopes.{DeclarationInsideScope, ParentScope}
import constraints.types.DeclarationOfType
import constraints.types.objects.Type
import constraints.{Constraint, ConstraintBuilder}

class Let(name: String, bindingValue: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Seq[Constraint] = {
    val scope = builder.newScope()
    val bindingType = builder.typeVariable()
    val declaration = NamedDeclaration(name, this)
    Seq(DeclarationOfType(declaration, bindingType), DeclarationInsideScope(declaration, scope), ParentScope(scope, parentScope)) ++
      bindingValue.constraints(builder, bindingType, parentScope) ++
      value.constraints(builder, _type, scope)
  }
}
