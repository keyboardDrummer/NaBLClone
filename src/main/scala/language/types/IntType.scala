package language.types

import bindingTypeMachine._
import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.{BoolConstraintType, IntConstraintType, LongConstraintType, Type}
import constraints.{Constraint, ConstraintBuilder}

object LongType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, LongConstraintType)

  override def evaluate(machine: Machine): MachineType = LongMachineType

  override def variables: Set[LanguageTypeVariable] = Set.empty
}

object IntType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, IntConstraintType)

  override def evaluate(machine: Machine): MachineType = IntMachineType
  override def variables: Set[LanguageTypeVariable] = Set.empty
}

object BoolType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, BoolConstraintType)

  override def evaluate(machine: Machine): MachineType = BoolMachineType
  override def variables: Set[LanguageTypeVariable] = Set.empty
}