package language.structs

import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.{StructType, Type}
import constraints.{Constraint, ConstraintBuilder}
import language.expressions.Expression

class Access(target: Expression, field: String) extends Expression
{
  /* We don't need a scope import because we can directly use the struct scope to resolve the member.
   */
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    val fieldDeclaration = builder.declarationVariable(_type)
    val structScope = builder.declaredScopeVariable(structDeclaration)
    builder.reference(field, this, structScope, fieldDeclaration)
    target.constraints(builder, StructType(structDeclaration), scope)
  }
}
