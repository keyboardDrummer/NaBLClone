package language.modules

import bindingTypeMachine.Machine
import constraints.objects.Reference
import constraints.scopes.ReferenceInScope
import constraints.scopes.imports.ScopeImport
import constraints.scopes.objects.Scope
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}

class ModuleImport(name: String) {

  def evaluate(machine: Machine): Unit = {
    val importedModule = machine.resolveModule(name)
    importedModule.initialize()
    machine.currentModule.imports ::= importedModule
  }

  def constraints(builder: ConstraintBuilder, scope: Scope): Unit = {
    val importedDeclaration = builder.declarationVariable()
    val importedScope = builder.declaredScopeVariable(importedDeclaration)
    builder.reference(name, this, scope, importedDeclaration)
    builder.add(ScopeImport(scope, importedScope))
  }
}
