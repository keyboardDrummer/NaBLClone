package constraints.types.objects

import constraints.Factory
import constraints.objects._

trait Type {
  def specialize(factory: Factory): Type

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
  def variables: Set[TypeVariable]
}

trait ConcreteType extends Type
