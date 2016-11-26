package language.structs

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.scopes.objects.Scope
import constraints.types.InstantiateScopeConstraint
import constraints.types.objects.{StructType, Type}
import language.expressions.Expression
import language.types.LanguageType

class New(structName: String, fieldInitializers: Seq[StructFieldInit], typeArgument: Option[LanguageType] = None) extends Expression
{
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    val structScope = builder.declaredScopeVariable(structDeclaration)
    builder.reference(structName, this, parentScope, structDeclaration)
    builder.typesAreEqual(_type, StructType(structDeclaration))
    val fieldStructType = typeArgument.fold(structScope)(t => {
        val instance = builder.typeVariable()
        t.constraints(builder, instance, parentScope)
        val instantiatedScope = builder.scopeVariable()
        builder.add(InstantiateScopeConstraint(instantiatedScope, structScope))
        instantiatedScope
    })
    fieldInitializers.foreach(value => value.constraints(builder, fieldStructType, parentScope))
  }

  override def evaluate(machine: Machine): MachineType = {
    val structType = machine.resolveStruct(structName)
    val instantiatedStructType = typeArgument.fold(structType)(t => structType.instantiate(structType.parameterType.get, t.evaluate(machine)))

    fieldInitializers.foreach(fieldInit => {
      val fieldType = instantiatedStructType.resolve(fieldInit.fieldName)
      machine.assertSubType(fieldInit.value.evaluate(machine), fieldType)
    })
    instantiatedStructType
  }
}
