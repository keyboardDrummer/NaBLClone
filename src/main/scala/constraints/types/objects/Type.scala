package constraints.types.objects

import bindingTypeMachine._
import constraints.{ConstraintBuilder, Factory}
import constraints.objects._
import constraints.scopes.objects.Scope
import language.expressions.Expression

trait Type {
  def specialize(mapping: Map[TypeVariable, TypeVariable]): Type

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {}
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

/*
The idea here is analogous to the machine closure type, where you store a reference to the concrete program in the type, and you type check that program
each type you apply the closure type. In constraint terms, this means generating new constraints each time you apply the ConstraintClosureType.
 */
case class ConstraintClosureType(parentScope: Scope, reference: Reference, body: ConstraintExpression) extends ConcreteType {
  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = this

  override def variables: Set[TypeVariable] = Set.empty

  override def instantiateType(variable: TypeVariable, instance: Type): Type = this

  override def fullyApplied: Boolean = true
}

trait ConstraintExpression
{
  def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit
}
