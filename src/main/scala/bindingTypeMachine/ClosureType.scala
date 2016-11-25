package bindingTypeMachine

case class ClosureType(scope: VariableScope, name: String, getType: Machine => MachineType) extends MachineType {
}

case class StructMachineType(name: String, fields: Map[String, MachineType]) extends MachineType

case class FunctionType(input: MachineType, output: MachineType) extends MachineType
