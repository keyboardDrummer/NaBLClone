package bindingTypeMachine

case class MachineTypeVariable(name: String) extends MachineType {
  override def instantiate(name2: String, value: MachineType): MachineType = if (name == name2) value else this

  override def variables: Set[MachineTypeVariable] = Set(this)
}
