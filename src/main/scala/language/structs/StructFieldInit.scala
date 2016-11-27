package language.structs

import constraints.objects.Reference
import constraints.scopes.ReferenceInScope
import constraints.scopes.objects.Scope
import constraints.types.DeclarationOfType
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}
import language.expressions.Expression

class StructFieldInit(val fieldName: String, val value: Expression) {
  def constraints(builder: ConstraintBuilder, structScope: Scope, parentScope: Scope): Unit = {
    val fieldType = builder.typeVariable()
    val fieldDeclaration = builder.declarationVariable(fieldType)
    builder.reference(fieldName, this, structScope, fieldDeclaration)
    value.constraints(builder, fieldType, parentScope)
  }
}
