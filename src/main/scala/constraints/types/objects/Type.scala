package constraints.types.objects

import constraints.Factory
import constraints.objects._
import language.expressions.Expression

trait Type {
  def specialize(mapping: Map[TypeVariable, TypeVariable]): Type

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
  def variables: Set[TypeVariable]

  def instantiateType(variable: TypeVariable, instance: Type) : Type

  def fullyApplied: Boolean

  def function: Type = this
}

case class Poly(arguments: Seq[TypeVariable], body: Type) extends Type {
  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = body.specialize(mapping)

  override def variables: Set[TypeVariable] = Set.empty

  override def instantiateType(variable: TypeVariable, instance: Type): Type = this

  override def fullyApplied: Boolean = true
}

trait ConcreteType extends Type
