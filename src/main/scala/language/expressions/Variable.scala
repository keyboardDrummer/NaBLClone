package language.expressions

import bindingTypeMachine.{Machine, MachineType}
import constraints.objects.{DeclarationVariable, Reference}
import constraints.scopes.ReferenceInScope
import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import constraints.types.{DeclarationOfType, Specialization, TypesAreEqual}
import constraints.{Constraint, ConstraintBuilder, ResolvesTo}

class NoSpecializeVariable(val name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val declarationType = builder.typeVariable()
    val declaration: DeclarationVariable = builder.declarationVariable(declarationType)
    builder.reference(name, this, scope, declaration)
    builder.typesAreEqual(_type, declarationType)
  }

  override def toString = s"Variable($name)"

  override def evaluate(machine: Machine): MachineType = machine.resolve(name)
}

class Variable(val name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val declarationType = builder.typeVariable()
    val declaration: DeclarationVariable = builder.declarationVariable(declarationType)
    builder.reference(name, this, scope, declaration)
    builder.specialization(_type, declarationType, this)
  }

  override def toString = s"Variable($name)"

  override def evaluate(machine: Machine): MachineType = machine.resolve(name)
}
