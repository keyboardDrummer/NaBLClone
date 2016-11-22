package constraints.types

import constraints.types.objects.{ConcreteType, Poly, Type, TypeVariable}
import constraints.ConstraintSolver



case class Specialization(var specialized: Type, var template: Type, debugInfo: Any = null) extends TypeConstraint
{
  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    specialized = specialized.instantiateType(variable, instance)
    template = template.instantiateType(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(specialized)

  override def apply(solver: ConstraintSolver): Boolean = {
    template match {
      case poly: Poly =>
        val instantiatedTemplate = poly.specialize(poly.arguments.map(v => (v, solver.factory.typeVariable)).toMap)
        solver.unifyTypes(specialized, instantiatedTemplate)
      case _: ConcreteType =>
        solver.unifyTypes(specialized, template)
      case _ =>
        val constraintTypes = solver.constraints.flatMap(c => c.boundTypes)
        val constraintVariables: Set[TypeVariable] = constraintTypes.flatMap(t => t.variables).toSet
        if (constraintVariables.intersect(template.variables).isEmpty)
          solver.unifyTypes(specialized, template)
        else
          false
    }
  }
}
