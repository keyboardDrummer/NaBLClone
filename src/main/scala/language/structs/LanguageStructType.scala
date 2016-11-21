package language.structs

import constraints.objects.Reference
import constraints.scopes.ReferenceInScope
import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.{StructType, Type}
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}
import language.types.LanguageType

class LanguageStructType(name: String) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val structDeclaration = builder.declarationVariable()
    val reference: Reference = Reference(name, this)
    Seq(TypesAreEqual(_type, StructType(structDeclaration)),
      ResolvesTo(reference, structDeclaration),
      ReferenceInScope(reference, scope))
  }
}
