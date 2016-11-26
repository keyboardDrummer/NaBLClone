package bindingTypeMachine

case class FunctionType(input: MachineType, output: MachineType) extends MachineType {
  override def instantiate(name: String, value: MachineType): MachineType = FunctionType(input.instantiate(name, value), output.instantiate(name, value))

  override def variables: Set[MachineTypeVariable] = input.variables ++ output.variables
}



// case class ForAllType(variable: String, body: MachineType) extends MachineType
