package constraints.scopes.imports

import constraints.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import constraints.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import constraints.scopes.{Declares, ScopeNode, _}
import constraints.ConstraintSolver

import scala.collection.mutable

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
      val edges = solver.scopeGraph.getOrElseUpdate(DeclarationNode(named), mutable.Set.empty[GraphEdge])
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
