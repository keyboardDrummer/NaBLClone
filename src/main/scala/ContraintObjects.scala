
case class Reference(name: String, id: AnyRef)

trait Declaration
case class DeclarationVariable(name: String) extends Declaration
case class NamedDeclaration(name: String, id: AnyRef) extends Declaration

trait Type

case class TypeVariable(name: String) extends Type
case class ConcreteType(constructor: String, arguments: Seq[Type]) extends Type
case class StructType(declaration: Declaration) extends Type