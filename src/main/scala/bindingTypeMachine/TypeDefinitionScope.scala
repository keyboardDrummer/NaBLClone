package bindingTypeMachine

case class TypeDefinitionScope(parent: VariableScope) extends VariableScope
{
  var typeParameter: Map[String, MachineType] = Map.empty

  def declareType(name: String, _type: MachineType): Unit = typeParameter += (name -> _type)
  def resolveType(name: String): MachineType = typeParameter.getOrElse[MachineType](name, parent.resolveType(name))

  override def resolve(name: String): MachineType = parent.resolve(name)

  override def currentModule: ModuleScope = parent.currentModule
}
