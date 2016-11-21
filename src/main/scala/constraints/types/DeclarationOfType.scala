package constraints.types

import constraints._
import constraints.objects.{Declaration, DeclarationVariable}
import constraints.types.objects.{Type, TypeVariable}

case class DeclarationOfType(var declaration: Declaration, var _type: Type) extends TypeConstraint {
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    _type.instantiateDeclaration(variable, instance)
    if (declaration == variable)
      declaration = instance
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (_type == variable)
      _type = instance
  }

  override def boundTypes: Set[Type] = Set(_type)

  override def apply(solver: ConstraintSolver): Boolean = {
    var result = true
    solver.environment = solver.environment.get(declaration).fold[Map[Declaration, Type]]({
      solver.environment + (declaration -> _type)
    })((existingType: Type) => {
      if (!solver.unifyTypes(existingType, _type)) {
        result = false
      }
      solver.environment
    })
    result
  }
}
