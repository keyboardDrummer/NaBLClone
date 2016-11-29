package language.expressions

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.{ConstraintExpression, Type}
import language.structs.{Access, StructFieldInit}

trait Expression extends ConstraintExpression {
  def evaluate(machine: Machine): MachineType

  def apply(argument: Expression) = Application(this, argument)
  def $: (Expression) => Application = apply

  def access(field: String) = new Access(this, field)

}

object Expression
{
  implicit def const(value: Int) : Const = Const(value)
  implicit def bool(value: Boolean) : BoolConst = BoolConst(value)
  implicit def variable(name: String) : Variable = new Variable(name)
}
