package language.expressions

import bindingTypeMachine.{Machine, MachineType}
import constraints._
import constraints.objects.DeclarationVariable
import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import modes.{ConstraintClosure, ConstraintHindleyMilner}

class Variable(val name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit =  builder.mode match {
    case ConstraintHindleyMilner =>
      val declaration: DeclarationVariable = builder.resolve(name, this, scope)
      val declarationType = builder.getType(declaration)
      builder.specialization(_type, declarationType, this)

    case ConstraintClosure =>
      builder.resolve(name, this, scope, Some(_type))
  }

  override def toString = s"Variable($name)"

  override def evaluate(machine: Machine): MachineType = machine.resolve(name)
}
