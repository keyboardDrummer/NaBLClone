package constraints.types

import constraints.ConstraintSolver
import constraints.types.objects.{ConcreteType, Type}

case class CheckSubType(var subType: Type, var superType: Type) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = {
    solver.canAssignTo(superType, subType)
  }
}

case class AssignSubType(var subType: Type, var superType: Type) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = (subType, superType) match {
    case (_:ConcreteType, _:ConcreteType) => solver.typeGraph.add(TypeNode(subType), SuperType(TypeNode(superType))); true
    case _ => false
  }
}