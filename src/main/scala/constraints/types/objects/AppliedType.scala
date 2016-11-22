package constraints.types.objects

import constraints.Factory

case class AppliedType(constructor: String, var arguments: Seq[Type]) extends ConcreteType {
  override def variables: Set[TypeVariable] = arguments.flatMap(t => t.variables).toSet

  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = AppliedType(constructor, arguments.map(a => a.specialize(mapping)))

  override def instantiateType(variable: TypeVariable, instance: Type): Type = {
    AppliedType(constructor, arguments.map(argument => argument.instantiateType(variable, instance)))
  }

  override def fullyApplied: Boolean = arguments.forall(a => a.fullyApplied)
}
