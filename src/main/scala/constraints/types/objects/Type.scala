package constraints.types.objects

import constraints.Factory
import constraints.objects._

trait Type {
  def specialize(mapping: Map[TypeVariable, TypeVariable]): Type

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
  def variables: Set[TypeVariable]

  def instantiateType(variable: TypeVariable, instance: Type) : Type

  def fullyApplied: Boolean
}

case class Poly(arguments: Seq[TypeVariable], body: Type) extends Type {
  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = body.specialize(mapping)

  override def variables: Set[TypeVariable] = Set.empty

  override def instantiateType(variable: TypeVariable, instance: Type): Type = this

  override def fullyApplied: Boolean = true
}

trait ConcreteType extends Type
