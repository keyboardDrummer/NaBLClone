package constraints.types

import constraints._
import constraints.objects.{Declaration, DeclarationVariable}
import constraints.types.objects.{Type, TypeVariable}

case class TypesAreEqual(var left: Type, var right: Type) extends TypeConstraint {

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    left.instantiateDeclaration(variable, instance)
    right.instantiateDeclaration(variable, instance)
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (left == variable)
      left = instance
    if (right == variable)
      right = instance
  }

  override def boundTypes: Set[Type] = Set(left, right)

  override def apply(solver: ConstraintSolver): Boolean = solver.unifyTypes(left,  right)
}
