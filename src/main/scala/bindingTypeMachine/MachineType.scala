package bindingTypeMachine

trait MachineType {
}

case class PrimitiveType(name: String) extends MachineType

object IntMachineType extends PrimitiveType("Int")
object LongMachineType extends PrimitiveType("Long")
object BoolMachineType extends PrimitiveType("Bool")

//case class Poly(arguments: Seq[TypeVariable], body: Type) extends Type {
//  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = body.specialize(mapping)
//
//  override def variables: Set[TypeVariable] = Set.empty
//
//  override def instantiateType(variable: TypeVariable, instance: Type): Type = this
//
//  override def fullyApplied: Boolean = true
//}


