package language.unionTypes

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.Type
import language.expressions.Expression
import language.structs.TypeDefinition
import language.types.LanguageType

case class UnionOption(name: String, _type: LanguageType)
case class UnionType(name: String, options: Seq[UnionOption]) extends TypeDefinition {
  override def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit = {
    val _type = builder.declarationType(name, this, parentScope)
    options.foreach(option => {
      builder.declaration(option.name, this, parentScope,
        Some(builder.getFunctionType(option._type.constraints(builder,parentScope), _type, this)))
    })
  }

  override def evaluate(machine: Machine): Unit = ???
}

case class Case(functionName: String, argumentName: String, body: Expression)
case class Match(subject: Expression, cases: Seq[Case]) extends Expression
{
  override def evaluate(machine: Machine): MachineType = ???

  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val subjectType = subject.getType(builder, parentScope)
    val caseTypes = cases.map(_case => {
      val bodyScope = builder.newScope(Some(parentScope))
      val argumentType = builder.declarationType(_case.argumentName, this, bodyScope)
      val bodyType = _case.body.getType(builder, bodyScope)
      builder.resolve(_case.functionName, this, parentScope, Some(builder.getFunctionType(argumentType, subjectType, this)))
      bodyType
    })
    caseTypes.foreach(t => builder.typesAreEqual(t, _type))
  }
}
