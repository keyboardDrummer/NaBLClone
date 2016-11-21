package language.structs

import constraints.objects.NamedDeclaration
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.objects.Scope
import constraints.scopes.{DeclarationInsideScope, ParentScope}
import constraints.types.DeclarationOfType
import constraints.{Constraint, ConstraintBuilder}

class Struct(name: String, fields: Seq[Field])
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Seq[Constraint] =
  {
    val scope = builder.newScope()
    val _type = builder.typeVariable()
    val structDeclaration: NamedDeclaration = NamedDeclaration(name, this)
    val fieldConstraints: Seq[Constraint] = fields.flatMap(field => {
      val fieldDeclaration: NamedDeclaration = NamedDeclaration(field.name, field)
      Seq(DeclarationInsideScope(fieldDeclaration, scope), DeclarationOfType(fieldDeclaration, _type)) ++ field._type.constraints(builder, _type, scope)
    })
    Seq(ParentScope(scope, parentScope), DeclarationOfScope(structDeclaration, scope),
      DeclarationInsideScope(structDeclaration, parentScope)) ++ fieldConstraints
  }
}
