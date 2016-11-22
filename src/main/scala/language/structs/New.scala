package language.structs

import constraints.objects.Reference
import constraints.scopes.ReferenceInScope
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.{StructType, Type}
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}
import language.expressions.Expression

class New(structName: String, values: Seq[StructFieldInit]) extends Expression
{
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    val structScope = builder.declaredScopeVariable(structDeclaration)
    builder.reference(structName, this, scope, structDeclaration)
    builder.typesAreEqual(_type, StructType(structDeclaration))
    values.foreach(value => value.constraints(builder, structScope, scope))
  }
}
