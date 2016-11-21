package constraints.types.objects

import constraints.Factory

case class ConcreteType(constructor: String, arguments: Seq[Type]) extends Type {
  override def variables: Set[TypeVariable] = arguments.flatMap(t => t.variables).toSet

  override def specialize(factory: Factory): Type = ConcreteType(constructor, arguments.map(a => factory.typeVariable))
}
