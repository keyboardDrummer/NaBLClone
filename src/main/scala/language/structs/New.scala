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
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val structDeclaration = builder.declarationVariable()
    val reference: Reference = Reference(structName, this)
    val structScope = builder.scopeVariable()
    Seq(ReferenceInScope(reference, scope),
      ResolvesTo(reference, structDeclaration),
      TypesAreEqual(_type, StructType(structDeclaration)),
      DeclarationOfScope(structDeclaration, structScope)) ++
        values.flatMap(value => value.constraints(builder, structScope, scope))
  }
}
