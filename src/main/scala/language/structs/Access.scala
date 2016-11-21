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
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val structDeclaration = builder.declarationVariable()
    val fieldDeclaration = builder.declarationVariable(_type)
    val structScope = builder.scopeVariable()
    builder.reference(field, this, structScope, fieldDeclaration)
    val targetType = builder.typeVariable()
    builder.getConstraints ++ Seq(TypesAreEqual(StructType(structDeclaration), targetType),
      DeclarationOfScope(structDeclaration, structScope)) ++
      target.constraints(builder, targetType, scope)
  }
}
