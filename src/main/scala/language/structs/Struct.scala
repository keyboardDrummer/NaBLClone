package language.structs

import bindingTypeMachine.{Machine, MachineType, MachineStructType, TypeCheckException}
import constraints.ConstraintBuilder
import constraints.objects.NamedDeclaration
import constraints.scopes.objects.Scope
import constraints.types.AssignSubType
import constraints.types.objects.StructType
import language.types.LanguageTypeVariable

class Struct(name: String, fields: Seq[Field], parent: Option[String] = None, typeParameter: Option[String] = None)
{
  def evaluate(machine: Machine): Unit = {

    val expectedVariables = typeParameter.fold(Set.empty[LanguageTypeVariable])(p => Set(LanguageTypeVariable(p)))

    val variables = fields.flatMap(field => field._type.variables).toSet
    if (variables != expectedVariables)
    {
      throw TypeCheckException("struct type variables don't add up")
    }

    val parentType = parent.map(p => machine.resolveStruct(p))

    val structType: MachineStructType = MachineStructType(name, fields.map(field => {
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
      builder.add(Seq(AssignSubType(StructType(structDeclaration), StructType(parentDeclaration))))
      scopeOfParent
    })
    val structScope = builder.declaredNewScope(structDeclaration, scopeOfParent)
    fields.foreach(field => {
      val _type = builder.typeVariable()
      builder.declaration(field.name, field, structScope, Some(_type))
      field._type.constraints(builder, _type, parentScope)
    })
  }
}
