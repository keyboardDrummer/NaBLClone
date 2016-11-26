package constraints.types.objects

case class PrimitiveType(name: String) extends ConcreteType {
  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = this

  override def variables: Set[TypeVariable] = Set.empty

  override def instantiateType(variable: TypeVariable, instance: Type): Type = this

  override def fullyApplied: Boolean = true
}

case class TypeApplication(function: Type, var arguments: Seq[Type]) extends ConcreteType {
  override def variables: Set[TypeVariable] = arguments.flatMap(t => t.variables).toSet

  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = TypeApplication(function, arguments.map(a => a.specialize(mapping)))

  override def instantiateType(variable: TypeVariable, instance: Type): Type = {
    TypeApplication(function, arguments.map(argument => argument.instantiateType(variable, instance)))
  }

  override def fullyApplied: Boolean = arguments.forall(a => a.fullyApplied)

  override def toString: String = function + (if (arguments.nonEmpty) "(" + arguments.map(a => a.toString).reduce((a, b) => a + ", " + b) + ")" else "")
}
