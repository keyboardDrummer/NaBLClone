package language.structs

import constraints.objects.Reference
import constraints.scopes.ReferenceInScope
import constraints.scopes.objects.Scope
import constraints.types.DeclarationOfType
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}
import language.expressions.Expression

case class StructFieldInit(fieldName: String, value: Expression) {
  def constraints(builder: ConstraintBuilder, structScope: Scope, parentScope: Scope): Unit = {
    val fieldDeclaration = builder.resolve(fieldName, this, structScope)
    val fieldType = builder.getType(fieldDeclaration)
    value.constraints(builder, fieldType, parentScope)
  }
}
