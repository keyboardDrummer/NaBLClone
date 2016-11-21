package constraints

import constraints.objects.{Declaration, DeclarationVariable, Reference}
import constraints.scopes._

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


