package constraints.types

import constraints.ConstraintSolver
import constraints.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import constraints.scopes.{DeclarationNode, DeclaresDeclaration, DeclaresScope, ScopeNode}
import constraints.scopes.objects.{Scope, ScopeVariable}
import constraints.types.objects.{Type, TypeVariable}

case class Copy(original: AnyRef, id: Int)

case class InstantiateDeclarationConstraint(instantiated: Declaration, template: Declaration) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = template match {
    case named:NamedDeclaration =>
      val scopeGraph = solver.scopeGraph
      val declaredScope = scopeGraph(DeclarationNode(named)).collect({ case x: DeclaresScope => x}).head.target
      val fieldDeclarations = scopeGraph(declaredScope).collect({ case x: DeclaresDeclaration => x})

      def copy(d: NamedDeclaration): NamedDeclaration = NamedDeclaration(d.name, solver.copy(d.id))

      val declarationCopy = copy(named)
      solver.declare(declarationCopy, solver.environment(named))

      val declaredScopeCopy = ScopeNode(solver.factory.freshScope)
      scopeGraph.add(DeclarationNode(declarationCopy), DeclaresScope(declaredScopeCopy))
      fieldDeclarations.foreach(d => {
        val originalDeclaration: NamedDeclaration = d.target.declaration
        val fieldDeclarationCopy: NamedDeclaration = copy(originalDeclaration)
        scopeGraph.add(declaredScopeCopy, DeclaresDeclaration(DeclarationNode(fieldDeclarationCopy)))
        solver.declare(fieldDeclarationCopy, solver.environment(originalDeclaration))
      })

      solver.unifyDeclarations(instantiated, declarationCopy)
    case _ => false
  }

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = super.instantiateDeclaration(variable, instance)

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = super.instantiateType(variable, instance)

  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = super.instantiateScope(variable, instance)

  override def boundTypes: Set[Type] = super.boundTypes
}
