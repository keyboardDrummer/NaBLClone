package language.structs

import bindingTypeMachine.{Machine, MachineType, StructMachineType}
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.objects.Scope
import constraints.types.{CheckSubType, TypesAreEqual}
import constraints.types.objects.{StructConstraintType, Type}
import constraints.{Constraint, ConstraintBuilder}
import language.expressions.Expression

case class TypeCase(typeName: String, bindingName: String, body: Expression)
case class MatchType(subject: Expression, cases: Seq[TypeCase]) extends Expression {
  override def evaluate(machine: Machine): MachineType = ???

  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val subjectType = subject.getType(builder, parentScope)
    cases.foreach(_case => {
      val caseTypeDeclaration = builder.resolve(_case.typeName, this, parentScope)
      val caseType = builder.getType(caseTypeDeclaration)
      builder.add(CheckSubType(caseType, subjectType))
      val bodyScope = builder.newScope(Some(parentScope))
      builder.declaration(_case.bindingName, this, bodyScope, Some(caseType))
      builder.typesAreEqual(_type, _case.body.getType(builder, bodyScope))
    })
  }
}

case class Access(target: Expression, field: String) extends Expression
{
  /* We don't need a scope import because we can directly use the struct scope to resolve the member.
   */
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    val fieldDeclaration = builder.declarationVariable(_type)
    val structScope = builder.declaredScopeVariable(structDeclaration)
    builder.reference(field, this, structScope, fieldDeclaration)
    target.constraints(builder, StructConstraintType(structDeclaration), scope)
  }

  override def evaluate(machine: Machine): MachineType = {
    val structType = target.evaluate(machine).asInstanceOf[StructMachineType]
    structType.resolve(field)
  }
}
