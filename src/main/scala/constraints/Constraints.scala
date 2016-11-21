package constraints

import constraints.scopeConstraints._

import scala.collection.mutable

trait Constraint {
  def apply(solver: ConstraintSolver): Boolean

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
  def instantiateType(variable: TypeVariable, instance: Type) = {}
  def instantiateScope(variable: ScopeVariable, instance: Scope) = {}
  def boundTypes: Set[Type] = Set.empty
}


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

  override def boundTypes: Set[Type] = Set(left, right)

  override def apply(solver: ConstraintSolver): Boolean = solver.unifyTypes(left,  right)
}

case class Specialization(var specialized: Type, var template: Type) extends TypeConstraint
{
  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (specialized == variable)
      specialized = instance
    if (template == variable)
      template = instance
  }

  override def boundTypes: Set[Type] = Set(specialized)

  override def apply(solver: ConstraintSolver): Boolean = {
    val constraintTypes = solver.constraints.diff(Seq(this)).flatMap(c => c.boundTypes) //TODO kan deze diff niet weg?
    val constraintVariables: Set[TypeVariable] = constraintTypes.flatMap(t => t.variables).toSet
    if (constraintVariables.intersect(template.variables).isEmpty)
    {
      val instantiatedTemplate = template.specialize(solver.factory)
      if (!solver.unifyTypes(specialized, instantiatedTemplate))
        return false
    }
    else
    {
      return false
    }
    true
  }
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

  override def boundTypes: Set[Type] = Set(_type)

  override def apply(solver: ConstraintSolver): Boolean = {
    var result = true
    solver.environment = solver.environment.get(declaration).fold[Map[Declaration, Type]]({
      solver.environment + (declaration -> _type)
    })((existingType: Type) => {
      if (!solver.unifyTypes(existingType, _type)) {
        result = false
      }
      solver.environment
    })
    result
  }
}

case class ReferenceInScope(reference: Reference, var scope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = scope match {
    case concrete: ConcreteScope => solver.graph.add(ReferenceNode(reference), ReferenceEdge(ScopeNode(concrete))); true
    case _ => false
  }
}

case class DeclarationInsideScope(var declaration: NamedDeclaration, var scope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = scope match {
    case concrete: ConcreteScope => solver.graph.add(ScopeNode(concrete), DeclaredIn(DeclarationNode(declaration))); true
    case _ => false
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

  override def apply(solver: ConstraintSolver): Boolean = declaration match {
    case named: NamedDeclaration =>
      val edges = solver.graph.getOrElseUpdate(DeclarationNode(named), mutable.Set.empty[GraphEdge])
      val declaredEdge = edges.find(e => e.isInstanceOf[Declares])
      if (declaredEdge.nonEmpty) {
        if (!solver.unifyScopes(declaredEdge.head.asInstanceOf[Declares].target.scope, scope)) {
          return false
        }
      }
      else {
        scope match {
          case c: ConcreteScope => edges.add(Declares(ScopeNode(c)))
          case _ => return false
        }
      }
      true
    case _ => false
  }
}

case class ResolvesTo(reference: Reference, var declaration: Declaration) extends ResolutionConstraint
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = {
    val resolvedDeclaration = solver.graph.resolve(reference)
    if (resolvedDeclaration != null)
    {
      if (!solver.unifyDeclarations(declaration, resolvedDeclaration))
      {
        throw new IllegalStateException("what?!")
      }
      true
    }
    else
    {
      false
    }
  }
}


