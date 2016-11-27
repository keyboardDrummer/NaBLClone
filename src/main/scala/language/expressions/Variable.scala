package language.expressions

import bindingTypeMachine.{Machine, MachineType}
import constraints._
import constraints.objects.DeclarationVariable
import constraints.scopes.objects.Scope
import constraints.types.objects.Type

class Variable(val name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit =  builder.mode match {
    case HindlerMilner =>
      val declarationType = builder.typeVariable()
      val declaration: DeclarationVariable = builder.declarationVariable(declarationType)
      builder.reference(name, this, scope, declaration)
      builder.specialization(_type, declarationType, this)
    case AbstractMachine =>
      val declarationType = builder.typeVariable()
      val declaration: DeclarationVariable = builder.declarationVariable(declarationType)
      builder.reference(name, this, scope, declaration)
      builder.typesAreEqual(_type, declarationType)
  }

  override def toString = s"Variable($name)"

  override def evaluate(machine: Machine): MachineType = machine.resolve(name)
}
