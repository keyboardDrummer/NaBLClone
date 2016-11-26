package language.types

import bindingTypeMachine._
import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.{BoolType, IntType, LongType, Type}
import constraints.{Constraint, ConstraintBuilder}

object LongLanguageType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope) = builder.typesAreEqual(_type, LongType)

  override def evaluate(machine: Machine): MachineType = LongMachineType

  override def variables: Set[LanguageTypeVariable] = Set.empty
}

object IntLanguageType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope) = builder.typesAreEqual(_type, IntType)

  override def evaluate(machine: Machine): MachineType = IntMachineType
  override def variables: Set[LanguageTypeVariable] = Set.empty
}

object BoolLanguageType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope) = builder.typesAreEqual(_type, BoolType)

  override def evaluate(machine: Machine): MachineType = BoolMachineType
  override def variables: Set[LanguageTypeVariable] = Set.empty
}