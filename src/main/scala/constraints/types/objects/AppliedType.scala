package constraints.types.objects

import constraints.Factory

case class AppliedType(constructor: String, arguments: Seq[Type]) extends ConcreteType {
  override def variables: Set[TypeVariable] = arguments.flatMap(t => t.variables).toSet

  override def specialize(factory: Factory): Type = AppliedType(constructor, arguments.map(a => factory.typeVariable))
}
