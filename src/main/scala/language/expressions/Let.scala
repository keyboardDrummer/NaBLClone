package language.expressions

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.Generalization
import constraints.types.objects.Type
import language.types.LanguageType
import modes.{ConstraintClosure, ConstraintHindleyMilner}

class Let(name: String, bindingValue: Expression, value: Expression, bindingLanguageType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = builder.mode match {
    case ConstraintHindleyMilner =>
      val scope = builder.newScope(Some(parentScope))
      val bindingType = bindingValue.constraints(builder, parentScope)
      val generalizedType = builder.declarationType(name, this, scope)
      builder.add(Generalization(generalizedType, bindingType))

      bindingLanguageType.foreach(t => {
        val bindingConstraintType = t.constraints(builder, parentScope)
        builder.typesAreEqual(bindingConstraintType, bindingType)
      })
      value.constraints(builder, _type, scope)

    case ConstraintClosure =>
      val scope = builder.newScope(Some(parentScope))
      val bindingType = bindingValue.constraints(builder, parentScope)
      builder.declaration(name, this, scope, Some(bindingType))
      value.constraints(builder, _type, scope)
  }

  override def evaluate(machine: Machine): MachineType = {
    val bindingType = bindingValue.evaluate(machine)
    bindingLanguageType.foreach(t => machine.assertSubType(bindingType, t.evaluate(machine)))
    machine.enterScope()
    machine.declare(name, bindingType)
    val result = value.evaluate(machine)
    machine.exitScope()
    result
  }
}
