package language.expressions

import bindingTypeMachine.{IntMachineType, LongMachineType, Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.{ConcreteType, IntConstraintType, LongConstraintType, Type}
import constraints.types.{CheckSubType, TypesAreEqual}

case class OverloadedAdd(left: Expression, right: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    builder.add(List(CheckSubType(_type, LongConstraintType)))
    left.constraints(builder, _type, scope)
    right.constraints(builder, _type, scope)
  }

  override def evaluate(machine: Machine): MachineType = {
    val leftType = left.evaluate(machine)
    val rightType = right.evaluate(machine)
    machine.assertSubType(leftType, LongMachineType)
    machine.assertEqual(leftType, rightType)
    leftType
  }
}

case class Add(left: Expression, right: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    builder.add(List(TypesAreEqual(_type, IntConstraintType)))
    left.constraints(builder, IntConstraintType, scope)
    right.constraints(builder, IntConstraintType, scope)
  }

  override def evaluate(machine: Machine): MachineType = {
    val leftType = left.evaluate(machine)
    val rightType = right.evaluate(machine)
    machine.assertEqual(IntMachineType, leftType)
    machine.assertEqual(IntMachineType, rightType)
    IntMachineType
  }
}
