package language.structs

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.{StructConstraintType, Type}
import language.types.{LanguageType, LanguageTypeVariable}

class StructType(name: String) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    builder.reference(name, this, scope, structDeclaration)
    builder.typesAreEqual(_type, StructConstraintType(structDeclaration))
  }

  override def evaluate(machine: Machine): MachineType = machine.resolveStruct(name)

  override def variables: Set[LanguageTypeVariable] = Set.empty
}
