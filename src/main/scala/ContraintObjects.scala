
case class Reference(name: String, id: AnyRef)

trait Declaration
case class DeclarationVariable(name: String) extends Declaration
case class NamedDeclaration(name: String, id: AnyRef) extends Declaration

trait Type {
  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
}

case class TypeVariable(name: String) extends Type
case class ConcreteType(constructor: String, arguments: Seq[Type]) extends Type
case class StructType(var declaration: Declaration) extends Type
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {
    if (declaration == variable)
      declaration = instance
  }
}