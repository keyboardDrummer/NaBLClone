package language.expressions

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.Generalization
import constraints.types.objects.Type
import language.types.LanguageType
import modes.{ConstraintChecker, ConstraintClosure, ConstraintHindleyMilner}

case class Let(name: String, bindingValue: Expression, value: Expression, bindingLanguageType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = builder.mode match {
    case _:ConstraintHindleyMilner =>
      val scope = builder.newScope(Some(parentScope))
      val bindingType = bindingValue.constraints(builder, scope)
      val generalizedType = builder.declarationType(name, this, scope)
      builder.add(Generalization(generalizedType, bindingType))

      bindingLanguageType.foreach(t => {
        val bindingConstraintType = t.constraints(builder, parentScope)
        builder.typesAreEqual(bindingConstraintType, bindingType)
      })
      value.constraints(builder, _type, scope)

    case _:ConstraintChecker =>
      val scope = builder.newScope(Some(parentScope))
      val bindingType = bindingValue.constraints(builder, scope)
      builder.declaration(name, this, scope, Some(bindingType))
      value.constraints(builder, _type, scope)
  }

  override def evaluate(machine: Machine): MachineType = {
    val bindingType = bindingLanguageType.fold(bindingValue.evaluate(machine))(t => {
      machine.enterScope()
      val bindingSpecifiedType = t.evaluate(machine)
      machine.declare(name, bindingSpecifiedType)
      val bindingType = bindingValue.evaluate(machine)
      machine.exitScope()
      machine.assertSubType(bindingType, bindingSpecifiedType)
      bindingType
    })
    machine.enterScope()
    machine.declare(name, bindingType)
    val result = value.evaluate(machine)
    machine.exitScope()
    result
  }
}
