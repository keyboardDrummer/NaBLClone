package constraints


case class Reference(name: String, id: AnyRef)

trait Declaration
case class DeclarationVariable(name: String) extends Declaration
case class NamedDeclaration(name: String, id: AnyRef) extends Declaration

trait Type {
  def specialize(factory: Factory): Type

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
  def variables: Set[TypeVariable]
}

case class TypeVariable(name: String) extends Type {
  override def variables: Set[TypeVariable] = Set(this)

  override def specialize(factory: Factory): Type = factory.typeVariable
}
case class ConcreteType(constructor: String, arguments: Seq[Type]) extends Type {
  override def variables: Set[TypeVariable] = arguments.flatMap(t => t.variables).toSet

  override def specialize(factory: Factory): Type = ConcreteType(constructor, arguments.map(a => factory.typeVariable))
}
case class StructType(var declaration: Declaration) extends Type
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {
    if (declaration == variable)
      declaration = instance
  }

  override def variables: Set[TypeVariable] = Set.empty

  override def specialize(factory: Factory): Type = this
}