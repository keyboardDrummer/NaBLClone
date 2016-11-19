import scala.collection.mutable


trait Constraint {
  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
  def instantiateType(variable: TypeVariable, instance: Type) = {}
}

trait ScopeConstraint extends Constraint
trait TypeConstraint extends Constraint
trait ResolutionConstraint extends Constraint

case class TypesAreEqual(var left: Type, var right: Type) extends TypeConstraint {
  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (left == variable)
      left = instance
    if (right == variable)
      right = instance
  }
}

case class DeclarationOfType(var declaration: Declaration, var _type: Type) extends TypeConstraint {
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (_type == variable)
      _type = instance
  }
}


trait Scope
case class ConcreteScope(number: Int) extends Scope
case class ScopeVariable(name: String) extends Scope

case class ReferenceInScope(reference: Reference, scope: Scope) extends ScopeConstraint

case class DeclarationInsideScope(var declaration: NamedDeclaration, scope: Scope) extends ScopeConstraint

case class ParentScope(child: Scope, parent: Scope) extends ScopeConstraint


case class ScopeImport(importingScope: Scope, importedScope: Scope) extends ScopeConstraint

case class DeclarationOfScope(var declaration: Declaration, scope: Scope) extends ResolutionConstraint
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }
}

case class ResolvesTo(reference: Reference, var declaration: Declaration) extends ResolutionConstraint
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }
}


