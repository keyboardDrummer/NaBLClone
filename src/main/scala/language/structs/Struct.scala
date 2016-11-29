package language.structs

import bindingTypeMachine.{Machine, MachineType, StructMachineType, TypeCheckException}
import constraints.ConstraintBuilder
import constraints.objects.NamedDeclaration
import constraints.scopes.objects.Scope
import constraints.types.AssignSubType
import constraints.types.objects.StructConstraintType
import language.types.LanguageTypeVariable

case class Struct(name: String, fields: Seq[Field], parent: Option[String] = None, typeParameter: Option[String] = None)
{
  def evaluate(machine: Machine): Unit = {

    val expectedVariables = typeParameter.fold(Set.empty[LanguageTypeVariable])(p => Set(LanguageTypeVariable(p)))

    val variables = fields.flatMap(field => field._type.variables).toSet
    if (variables != expectedVariables)
    {
      throw TypeCheckException(s"struct type variables don't add up, variables are $variables but expected $expectedVariables")
    }

    val parentType = parent.map(p => machine.resolveStruct(p))

    val structType: StructMachineType = StructMachineType(name, fields.map(field => {
      (field.name, field._type.evaluate(machine))
    }).toMap, parentType, typeParameter)

    parentType.foreach(p => machine.addSubType(structType, p))
    machine.declareStruct(structType)
  }

  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit =
  {
    val structDeclaration: NamedDeclaration = builder.declaration(name, this, parentScope)
    val scopeOfParent: Option[Scope] = parent.map(p => {
      val parentDeclaration = builder.declarationVariable()
      val scopeOfParent = builder.declaredScopeVariable(parentDeclaration)
      builder.reference(p, this, parentScope, parentDeclaration)
      builder.add(List(AssignSubType(StructConstraintType(structDeclaration), StructConstraintType(parentDeclaration))))
      scopeOfParent
    })
    val structScope = builder.declaredNewScope(structDeclaration, scopeOfParent)
    fields.foreach(field => {
      val _type = field._type.constraints(builder, parentScope)
      builder.declaration(field.name, field, structScope, Some(_type))
    })
  }

  override def toString = s"Struct($name)"
}
