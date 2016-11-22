package constraints.types

import constraints.types.objects.{Type, TypeVariable}
import constraints.ConstraintSolver

case class Specialization(var specialized: Type, var template: Type) extends TypeConstraint
{
  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    specialized = specialized.instantiateType(variable, instance)
    template = template.instantiateType(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(specialized)

  override def apply(solver: ConstraintSolver): Boolean = {
    val constraintTypes = solver.constraints.diff(Seq(this)).flatMap(c => c.boundTypes) //TODO kan deze diff niet weg?
    val constraintVariables: Set[TypeVariable] = constraintTypes.flatMap(t => t.variables).toSet
    if (constraintVariables.intersect(template.variables).isEmpty) //TODO misschien kan dit bepalen of een type unconstrained is wel al wanneer het in de environment gestopt wordt. dan hoeft het maar een keer.
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
