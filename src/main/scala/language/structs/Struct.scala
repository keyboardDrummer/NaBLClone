package language.structs

import constraints.objects.NamedDeclaration
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.objects.Scope
import constraints.scopes.{DeclarationInsideScope, ParentScope}
import constraints.types.DeclarationOfType
import constraints.{Constraint, ConstraintBuilder}

class Struct(name: String, fields: Seq[Field], parent: Option[String] = None)
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Seq[Constraint] =
  {
    val scopeOfParent: Option[Scope] = parent.map(p => {
      val parentDeclaration = builder.declarationVariable()
      val scopeOfParent = builder.declaredScopeVariable(parentDeclaration)
      builder.reference(p, this, parentScope, parentDeclaration)
      scopeOfParent
    })
    val structScope = builder.newScope(scopeOfParent)
    val structDeclaration: NamedDeclaration = builder.declaration(name, this, parentScope)
    val fieldConstraints: Seq[Constraint] = fields.flatMap(field => {
      val _type = builder.typeVariable()
      builder.declaration(field.name, field, structScope, Some(_type))
      field._type.constraints(builder, _type, parentScope)
    })
    builder.getConstraints ++ Seq(DeclarationOfScope(structDeclaration, structScope)) ++ fieldConstraints
  }
}
