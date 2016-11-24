package language.structs

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.{StructType, Type}
import language.types.LanguageType

class LanguageStructType(name: String) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope) = {
    val structDeclaration = builder.declarationVariable()
    builder.reference(name, this, scope, structDeclaration)
    builder.typesAreEqual(_type, StructType(structDeclaration))
  }

  override def evaluate(machine: Machine): MachineType = machine.resolveStruct(name)
}
