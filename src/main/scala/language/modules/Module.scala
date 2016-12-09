package language.modules

import bindingTypeMachine.Machine
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import language.structs.TypeDefinition

case class Module(name: String, bindings: Seq[Binding], structs: Seq[TypeDefinition] = Seq.empty, imports: Seq[ModuleImport] = Seq.empty)
{

  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit = {
    val moduleDeclaration = builder.declaration(name, this, parentScope)
    val scope = builder.declaredNewScope(moduleDeclaration, Some(parentScope))
    structs.foreach(struct => struct.constraints(builder, scope))
    bindings.foreach(binding => binding.constraints(builder, scope))
    imports.foreach(_import => _import.constraints(builder, scope)) //TODO moet bovenaan
  }

  def bind(machine: Machine): Unit = {
    machine.newModule(this)
  }

  def evaluate(machine: Machine): Unit = {
    val moduleScope = machine.modules(name)
    moduleScope.initialize()
  }
}
