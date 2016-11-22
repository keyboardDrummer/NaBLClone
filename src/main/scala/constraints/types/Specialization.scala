package constraints.types

import constraints.types.objects.{Type, TypeVariable}
import constraints.ConstraintSolver

case class Specialization(var specialized: Type, var template: Type) extends TypeConstraint
{
  var instantiatedTemplate: Option[Type] = None //this is a pretty weird hack to make sure that when instantiating types in unifyTypes, all types are correctly updated.
  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    specialized = specialized.instantiateType(variable, instance)
    template = template.instantiateType(variable, instance)
    instantiatedTemplate = instantiatedTemplate.map(t => t.instantiateType(variable, instance))
  }

  override def boundTypes: Set[Type] = Set(specialized)

  override def apply(solver: ConstraintSolver): Boolean = {
    val constraintTypes = solver.constraints.diff(Seq(this)).flatMap(c => c.boundTypes) //TODO kan deze diff niet weg?
    val constraintVariables: Set[TypeVariable] = constraintTypes.flatMap(t => t.variables).toSet
    if (constraintVariables.intersect(template.variables).isEmpty) //TODO misschien kan dit bepalen of een type unconstrained is wel al wanneer het in de environment gestopt wordt. dan hoeft het maar een keer.
    {
      val mapping = template.variables.map(v => (v,solver.factory.typeVariable)).toMap
      instantiatedTemplate = Some(instantiatedTemplate.getOrElse(template.specialize(mapping)))
      if (!solver.unifyTypes(specialized, instantiatedTemplate.get))
        return false
    }
    else
    {
      return false
    }
    true
  }
}
