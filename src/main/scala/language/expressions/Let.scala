package language.expressions

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.Generalization
import constraints.types.objects.Type
import language.types.LanguageType

class NoGeneralizeLet(name: String, bindingValue: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val scope = builder.newScope(Some(parentScope))
    val bindingType = builder.typeVariable()
    builder.declaration(name, this, scope, Some(bindingType))
    bindingValue.constraints(builder, bindingType, parentScope)
    value.constraints(builder, _type, scope)
  }

  override def evaluate(machine: Machine): MachineType = new Let(name, bindingValue, value).evaluate(machine)
}

class Let(name: String, bindingValue: Expression, value: Expression, bindingLanguageType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val scope = builder.newScope(Some(parentScope))
    val bindingType = builder.typeVariable()
    val generalizedType = builder.typeVariable()
    builder.add(Generalization(generalizedType, bindingType))
    builder.declaration(name, this, scope, Some(generalizedType))
    bindingValue.constraints(builder, bindingType, parentScope)
    bindingLanguageType.foreach(t => {
      val bindingConstraintType = builder.typeVariable()
      t.constraints(builder, bindingConstraintType, parentScope)
      builder.typesAreEqual(bindingConstraintType, bindingType)
    })
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
