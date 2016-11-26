package bindingTypeMachine

case class ClosureType(scope: VariableScope, name: String, getType: Machine => MachineType) extends MachineType {
  override def instantiate(target: String, value: MachineType): MachineType = ClosureType(scope, name, m => getType(m).instantiate(target, value))

  override def variables: Set[MachineTypeVariable] = Set.empty
}




