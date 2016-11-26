package bindingTypeMachine

trait MachineType {
  def instantiate(target: String, value: MachineType): MachineType
  def variables: Set[MachineTypeVariable]
}

case class PrimitiveMachineType(name: String) extends MachineType {
  override def instantiate(name: String, value: MachineType): MachineType = this

  override def variables: Set[MachineTypeVariable] = Set.empty
}

object IntMachineType extends PrimitiveMachineType("Int")
object LongMachineType extends PrimitiveMachineType("Long")
object BoolMachineType extends PrimitiveMachineType("Bool")

//case class Poly(arguments: Seq[TypeVariable], body: Type) extends Type {
//  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = body.specialize(mapping)
//
//  override def variables: Set[TypeVariable] = Set.empty
//
//  override def instantiateType(variable: TypeVariable, instance: Type): Type = this
//
//  override def fullyApplied: Boolean = true
//}


