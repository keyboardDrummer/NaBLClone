package language.modules

import constraints.objects.Reference
import constraints.scopes.ReferenceInScope
import constraints.scopes.imports.ScopeImport
import constraints.scopes.objects.Scope
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}

class ModuleImport(name: String) {
  def constraints(builder: ConstraintBuilder, scope: Scope): Seq[Constraint] = {
    val importedDeclaration = builder.declarationVariable()
    val importedScope = builder.declaredScopeVariable(importedDeclaration)
    val reference: Reference = Reference(name, this)
    Seq(ReferenceInScope(reference, scope), ResolvesTo(reference, importedDeclaration)) ++ builder.getConstraints ++  Seq(ScopeImport(scope, importedScope))
  }
}
