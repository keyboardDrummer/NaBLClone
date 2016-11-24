package language.modules

import bindingTypeMachine.Machine
import constraints.objects.Reference
import constraints.scopes.ReferenceInScope
import constraints.scopes.imports.ScopeImport
import constraints.scopes.objects.Scope
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}

class ModuleImport(name: String) {

  def evaluate(machine: Machine) = {
    val importedModule = machine.resolve(name, this)
    machine.importScope(importedModule)
  }

  def constraints(builder: ConstraintBuilder, scope: Scope) = {
    val importedDeclaration = builder.declarationVariable()
    val importedScope = builder.declaredScopeVariable(importedDeclaration)
    builder.reference(name, this, scope, importedDeclaration)
    builder.add(ScopeImport(scope, importedScope))
  }
}
