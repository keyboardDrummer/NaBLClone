package constraints.types

import constraints.ConstraintSolver
import constraints.objects.{Declaration, DeclarationVariable}
import constraints.scopes.objects.{Scope, ScopeVariable}
import constraints.types.objects.{Type, TypeVariable}

case class InstantiateScopeConstraint(instantiated: ScopeVariable, template: Scope) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = {

  }

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = super.instantiateDeclaration(variable, instance)

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = super.instantiateType(variable, instance)

  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = super.instantiateScope(variable, instance)

  override def boundTypes: Set[Type] = super.boundTypes
}
