package bindingTypeMachine

case class ClosureType(scope: VariableScope, name: String, getType: Machine => MachineType) extends MachineType {
}

case class StructMachineType(name: String, fields: Map[String, MachineType], parent: Option[StructMachineType] = None) extends MachineType
{
  def resolve(field: String): MachineType = fields.get(field).orElse(parent.map(p => p.resolve(field))).get
}

case class FunctionType(input: MachineType, output: MachineType) extends MachineType
