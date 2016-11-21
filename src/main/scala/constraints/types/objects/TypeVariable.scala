package constraints.types.objects

import constraints.Factory

case class TypeVariable(name: String) extends Type {
  override def variables: Set[TypeVariable] = Set(this)

  override def specialize(factory: Factory): Type = factory.typeVariable
}
