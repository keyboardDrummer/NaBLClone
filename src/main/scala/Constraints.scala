


trait Constraint {
  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
  def instantiateType(variable: TypeVariable, instance: Type) = {}
  def instantiateScope(variable: ScopeVariable, instance: Scope) = {}
  def types: Set[Type] = Set.empty
}

trait ScopeConstraint extends Constraint
trait TypeConstraint extends Constraint
trait ResolutionConstraint extends Constraint

case class TypesAreEqual(var left: Type, var right: Type) extends TypeConstraint {

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    left.instantiateDeclaration(variable, instance)
    right.instantiateDeclaration(variable, instance)
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (left == variable)
      left = instance
    if (right == variable)
      right = instance
  }

  override def types: Set[Type] = Set(left, right)
}

case class Specialization(var specialized: Type, var template: Type) extends TypeConstraint
{
  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (specialized == variable)
      specialized = instance
    if (template == variable)
      template = instance
  }

  override def types: Set[Type] = Set(specialized, template)
}

case class DeclarationOfType(var declaration: Declaration, var _type: Type) extends TypeConstraint {
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    _type.instantiateDeclaration(variable, instance)
    if (declaration == variable)
      declaration = instance
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (_type == variable)
      _type = instance
  }

  override def types: Set[Type] = Set(_type)
}


trait Scope
case class ConcreteScope(number: Int) extends Scope
case class ScopeVariable(name: String) extends Scope

case class ReferenceInScope(reference: Reference, var scope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
  }
}

case class DeclarationInsideScope(var declaration: NamedDeclaration, var scope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
  }
}

case class ParentScope(var child: Scope, var parent: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (child == variable)
      child = instance
    if (parent == variable)
      parent = instance
  }
}

case class ScopeImport(var importingScope: Scope, var importedScope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (importingScope == variable)
      importingScope = instance
    if (importedScope == variable)
      importedScope = instance
  }
}

case class DeclarationOfScope(var declaration: Declaration, var scope: Scope) extends ResolutionConstraint
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }

  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
  }
}

case class ResolvesTo(reference: Reference, var declaration: Declaration) extends ResolutionConstraint
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }
}


