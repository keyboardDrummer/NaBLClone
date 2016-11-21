package constraints.types

import constraints.ConstraintSolver
import constraints.types.objects.{ConcreteType, Type, TypeVariable}

case class CheckSubType(var subType: Type, var superType: Type) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = {
    solver.canAssignTo(superType, subType)
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (variable == subType)
      subType = instance
    if (variable == superType)
      superType = instance
  }

  override def boundTypes: Set[Type] = Set(subType, superType)
}

case class AssignSubType(var subType: Type, var superType: Type) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = (subType, superType) match {
    case (_:ConcreteType, _:ConcreteType) => solver.typeGraph.add(TypeNode(subType), SuperType(TypeNode(superType))); true
    case _ => false
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    if (variable == subType)
      subType = instance
    if (variable == superType)
      superType = instance
  }

  override def boundTypes: Set[Type] = Set(subType, superType)
}