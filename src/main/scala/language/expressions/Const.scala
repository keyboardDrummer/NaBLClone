package language.expressions

import bindingTypeMachine._
import constraints.scopes.objects.Scope
import constraints.types.TypesAreEqual
import constraints.types.objects.{ConcreteType, Type}
import constraints.{Constraint, ConstraintBuilder}
import language.types.{BoolType, IntType, LongType}

case class LongConst(value: Long) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, LongType)

  override def evaluate(machine: Machine): MachineType = LongMachineType
}

case class Const(value: Int) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, IntType)

  override def evaluate(machine: Machine): MachineType = IntMachineType
}

case class BoolConst(value: Boolean) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, BoolType)

  override def evaluate(machine: Machine): MachineType = BoolMachineType
}
