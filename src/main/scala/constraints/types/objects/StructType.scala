package constraints.types.objects

import constraints.Factory
import constraints.objects.{Declaration, DeclarationVariable}

case class StructType(var declaration: Declaration) extends Type
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {
    if (declaration == variable)
      declaration = instance
  }

  override def variables: Set[TypeVariable] = Set.empty

  override def specialize(factory: Factory): Type = this
}
