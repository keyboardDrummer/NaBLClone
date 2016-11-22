package language.modules

import constraints.objects.Reference
import constraints.scopes.ReferenceInScope
import constraints.scopes.imports.ScopeImport
import constraints.scopes.objects.Scope
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}

class ModuleImport(name: String) {
  def constraints(builder: ConstraintBuilder, scope: Scope) = {
    val importedDeclaration = builder.declarationVariable()
    val importedScope = builder.declaredScopeVariable(importedDeclaration)
    builder.reference(name, this, scope, importedDeclaration)
    builder.add(ScopeImport(scope, importedScope))
  }
}
