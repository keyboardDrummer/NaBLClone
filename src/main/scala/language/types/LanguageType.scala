package language.types

import bindingTypeMachine.{Machine, MachineType, MachineTypeVariable, TypeCheckException}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.Type

trait LanguageType
{
  def variables: Set[LanguageTypeVariable]

  def evaluate(machine: Machine): MachineType

  def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope)

  def constraints(builder: ConstraintBuilder, scope: Scope) : Type = {
    val result = builder.typeVariable()
    constraints(builder, result, scope)
    result
  }
}

object LanguageType
{
  implicit def variable(name: String) : LanguageTypeVariable = LanguageTypeVariable(name)
}

case class LanguageTypeVariable(name: String) extends LanguageType {
  override def evaluate(machine: Machine): MachineType = MachineTypeVariable(name)

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val typeVariable = builder.typeVariables.getOrElseUpdate(name, builder.typeVariable())
    builder.typesAreEqual(typeVariable, _type)
  }

  override def variables: Set[LanguageTypeVariable] = Set(this)
}

case class LanguageForAllType(variable: String, body: LanguageType) extends LanguageType {
  override def evaluate(machine: Machine): MachineType = {
    if (body.variables != Set(LanguageTypeVariable(variable)))
    {
      throw TypeCheckException("language forall variables don't add up")
    }
    body.evaluate(machine)
  }

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    if (body.variables != Set(LanguageTypeVariable(variable)))
    {
      throw TypeCheckException("language forall variables don't add up") //TODO gekke check tijdens constraint collectie
    }
    body.constraints(builder, _type, scope)
  }

  override def variables: Set[LanguageTypeVariable] = body.variables.diff(Set(LanguageTypeVariable(variable)))
}
