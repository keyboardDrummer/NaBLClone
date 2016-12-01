package bindingTypeMachine

case class ExpressionScope(parent: VariableScope) extends VariableScope
{
  def declare(name: String, _type: MachineType): Unit = {
    environment += name -> _type
  }

  var environment: Map[String, MachineType] = Map.empty

  def resolve(name: String): MachineType = environment.getOrElse(name, parent.resolve(name))

  override def currentModule: ModuleScope = parent.currentModule

  override def resolveType(name: String): MachineType = parent.resolveType(name)
}
