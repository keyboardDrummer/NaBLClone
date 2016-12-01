package bindingTypeMachine

import language.modules.Module

case class ModuleScope(machine: Machine, module: Module) extends VariableScope
{
  private var initialized: Boolean = false
  def initialize(): Unit = {
    if (!initialized)
    {
      initialized = true
      machine.currentScope = this
      module.imports.foreach(_import => _import.evaluate(machine))
      module.structs.foreach(struct => struct.evaluate(machine))
      module.bindings.foreach(binding => binding.evaluate(machine))
    }
  }

  def resolveStruct(name: String): StructMachineType = structDefinitions(name)

  var structDefinitions: Map[String, StructMachineType] = Map.empty
  var imports: List[ModuleScope] = List.empty
  var bindings: Map[String, MachineType] = Map.empty

  def addBinding(name: String, _type: MachineType): Unit = {
    bindings += name -> _type
  }

  override def resolve(name: String): MachineType = bindings.getOrElse(name, imports.map(i => i.resolve(name)).head)

  override def currentModule: ModuleScope = this

  override def resolveType(name: String): MachineType = resolveStruct(name)

  override def parent: VariableScope = throw new IllegalStateException()
}
