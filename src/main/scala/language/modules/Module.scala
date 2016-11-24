package language.modules

import bindingTypeMachine.Machine
import constraints.objects.NamedDeclaration
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.objects.Scope
import constraints.scopes.{DeclarationInsideScope, ParentScope}
import constraints.{Constraint, ConstraintBuilder}
import language.structs.Struct

class Module(name: String, bindings: Seq[Binding], structs: Seq[Struct] = Seq.empty, imports: Seq[ModuleImport] = Seq.empty)
{

  def constraints(builder: ConstraintBuilder, parentScope: Scope) = {
    val moduleDeclaration = builder.declaration(name, this, parentScope)
    val scope = builder.declaredNewScope(moduleDeclaration, Some(parentScope))
    structs.foreach(struct => struct.constraints(builder, scope))
    bindings.foreach(binding => binding.constraints(builder, scope))
    imports.foreach(_import => _import.constraints(builder, scope)) //TODO moet bovenaan
  }

  def evaluate(machine: Machine) = {
    machine.newModule(name)
    imports.foreach(_import => _import.evaluate(machine))
    structs.foreach(struct => struct.evaluate(machine))
    bindings.foreach(binding => binding.evaluate(machine))
  }
}
