package constraints.types

import constraints.ConstraintSolver
import constraints.types.objects._

case class Generalization(var generalized: Type, var template: Type) extends TypeConstraint
{
  override def apply(solver: ConstraintSolver): Boolean = {
    val constraintTypes = solver.constraints.diff(Seq(this)).flatMap(c => c.boundTypes)
    val constraintVariables: Set[TypeVariable] = constraintTypes.flatMap(t => t.variables).toSet
    if (constraintVariables.intersect(template.variables).isEmpty)
    {
      val instantiatedTemplate = Poly(template.variables.toSeq, template)
      return solver.unifyTypes(generalized, instantiatedTemplate)
    }
    false
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    generalized = generalized.instantiateType(variable, instance)
    template = template.instantiateType(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(generalized)
}
