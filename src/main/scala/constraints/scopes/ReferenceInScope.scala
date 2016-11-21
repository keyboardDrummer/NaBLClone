package constraints.scopes

import constraints.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import constraints.ConstraintSolver
import constraints.objects.Reference

case class ReferenceInScope(reference: Reference, var scope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = scope match {
    case concrete: ConcreteScope => solver.scopeGraph.add(ReferenceNode(reference), ReferenceEdge(ScopeNode(concrete))); true
    case _ => false
  }
}
