package bindingTypeMachine

trait MachineType {
  def instantiate(target: String, value: MachineType): MachineType
  def variables: Set[MachineTypeVariable]
}

case class MachineTypeReference(var reference: MachineType) extends MachineType {
  override def instantiate(target: String, value: MachineType): MachineType = reference.instantiate(target, value)

  override def variables: Set[MachineTypeVariable] = reference.variables
}

case class PrimitiveMachineType(name: String) extends MachineType {
  override def instantiate(name: String, value: MachineType): MachineType = this

  override def variables: Set[MachineTypeVariable] = Set.empty
}

object IntMachineType extends PrimitiveMachineType("Int")
object LongMachineType extends PrimitiveMachineType("Long")
object BoolMachineType extends PrimitiveMachineType("Bool")


