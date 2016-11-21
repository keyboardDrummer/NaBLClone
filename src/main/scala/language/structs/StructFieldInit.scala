package language.structs

import constraints.objects.Reference
import constraints.scopes.ReferenceInScope
import constraints.scopes.objects.Scope
import constraints.types.DeclarationOfType
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}
import language.expressions.Expression

class StructFieldInit(fieldName: String, value: Expression) {
  def constraints(builder: ConstraintBuilder, structScope: Scope, parentScope: Scope): Seq[Constraint] = {
    val reference = Reference(fieldName, this)
    val fieldDeclaration = builder.declarationVariable()
    val fieldType = builder.typeVariable()
    Seq(ReferenceInScope(reference, structScope), ResolvesTo(reference, fieldDeclaration),
      DeclarationOfType(fieldDeclaration, fieldType)) ++ value.constraints(builder, fieldType, parentScope)
  }
}
