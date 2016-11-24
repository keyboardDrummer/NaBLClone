package language.expressions

import bindingTypeMachine.Machine
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.{ConcreteType, Type}
import constraints.types.{CheckSubType, TypesAreEqual}
import language.types.{IntType, LongType}

case class OverloadedAdd(left: Expression, right: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    builder.add(Seq(CheckSubType(_type, LongType)))
    left.constraints(builder, _type, scope)
    right.constraints(builder, _type, scope)
  }

  override def evaluate(machine: Machine): ConcreteType = {
    val leftType = left.evaluate(machine)
    val rightType = right.evaluate(machine)
    machine.assertSubType(leftType, LongType)
    machine.assertEqual(leftType, rightType)
    leftType
  }
}

case class Add(left: Expression, right: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    builder.add(Seq(TypesAreEqual(_type, IntType)))
    left.constraints(builder, IntType, scope)
    right.constraints(builder, IntType, scope)
  }

  override def evaluate(machine: Machine): ConcreteType = {
    val leftType = left.evaluate(machine)
    val rightType = right.evaluate(machine)
    machine.assertEqual(IntType, leftType)
    machine.assertEqual(IntType, rightType)
    IntType
  }
}
