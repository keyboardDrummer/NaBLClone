package language.structs

import bindingTypeMachine.{Machine, MachineType, StructMachineType}
import constraints.ConstraintBuilder
import constraints.objects.NamedDeclaration
import constraints.scopes.objects.Scope
import constraints.types.AssignSubType
import constraints.types.objects.StructType

class Struct(name: String, fields: Seq[Field], parent: Option[String] = None)
{
  def evaluate(machine: Machine) = {
    val structType: StructMachineType = StructMachineType(name, fields.map(field => {
      (field.name, field._type.evaluate(machine))
    }).toMap)
    machine.declareStruct(structType)

    parent.foreach(p => {
      val parentType = machine.resolveStruct(p)
      machine.subType(structType, parentType)
    })
  }

  def constraints(builder: ConstraintBuilder, parentScope: Scope) =
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
