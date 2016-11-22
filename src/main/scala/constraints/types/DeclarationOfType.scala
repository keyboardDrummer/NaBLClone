package constraints.types

import constraints._
import constraints.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import constraints.types.objects.{Type, TypeVariable}

case class DeclarationOfType(var declaration: Declaration, var _type: Type) extends TypeConstraint {
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    _type.instantiateDeclaration(variable, instance)
    if (declaration == variable)
      declaration = instance
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    _type = _type.instantiateType(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(_type)

  override def apply(solver: ConstraintSolver): Boolean = declaration match {
    case named: NamedDeclaration =>
      var result = true
      val currentValue: Option[Type] = solver.environment.get(declaration)
      solver.environment = currentValue match {
        case Some(existingType) =>
          if (!solver.unifyTypes(existingType, _type)) {
            result = false
          }
          solver.environment
        case _ => solver.environment + (declaration -> _type)
      }
      result
    case _ => false
  }
}
