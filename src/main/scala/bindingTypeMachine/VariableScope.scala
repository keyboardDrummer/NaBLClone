package bindingTypeMachine

trait VariableScope
{
  def resolve(name: String): MachineType
  def currentModule : ModuleScope
  def resolveType(name: String): MachineType
  def parent: VariableScope
}
