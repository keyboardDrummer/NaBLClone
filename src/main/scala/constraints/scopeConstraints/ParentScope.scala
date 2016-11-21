package constraints.scopeConstraints

import constraints.ConstraintSolver

case class ParentScope(var child: Scope, var parent: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (child == variable)
      child = instance
    if (parent == variable)
      parent = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = (child, parent) match {
    case (concreteChild: ConcreteScope, concreteParent: ConcreteScope) => solver.graph.add(ScopeNode(concreteChild), Parent(ScopeNode(concreteParent))); true
    case _ => false
  }
}
