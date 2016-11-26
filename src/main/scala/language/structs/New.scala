package language.structs

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.objects.{StructType, Type}
import language.expressions.Expression
import language.types.LanguageType

class New(structName: String, fieldInitialisers: Seq[StructFieldInit], typeArgument: Option[LanguageType] = None) extends Expression
{
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    val structScope = builder.declaredScopeVariable(structDeclaration)
    builder.reference(structName, this, scope, structDeclaration)
    builder.typesAreEqual(_type, StructType(structDeclaration))
    fieldInitialisers.foreach(value => value.constraints(builder, structScope, scope))
  }

  override def evaluate(machine: Machine): MachineType = {
    val structType = machine.resolveStruct(structName)
    val instantiatedStructType = typeArgument.fold(structType)(t => structType.instantiate(structType.parameterType.get, t.evaluate(machine)))

    fieldInitialisers.foreach(fieldInit => {
      val fieldType = instantiatedStructType.resolve(fieldInit.fieldName)
      machine.assertSubType(fieldInit.value.evaluate(machine), fieldType)
    })
    instantiatedStructType
  }
}
