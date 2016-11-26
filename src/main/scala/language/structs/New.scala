package language.structs

import bindingTypeMachine.{Machine, MachineType}
import constraints.ConstraintBuilder
import constraints.objects.{Declaration, NamedDeclaration}
import constraints.scopes.objects.Scope
import constraints.types.InstantiateScopeConstraint
import constraints.types.objects.{StructType, Type, TypeApplication}
import language.expressions.Expression
import language.types.LanguageType

class New(structName: String, fieldInitializers: Seq[StructFieldInit], languageTypeArgument: Option[LanguageType] = None) extends Expression
{
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    val structType: Type = languageTypeArgument.fold[Type](StructType(structDeclaration))((t: LanguageType) => {
      val typeArgument = builder.typeVariable()
      t.constraints(builder, typeArgument, parentScope)
      val instantiatedStruct = builder.declarationVariable()
      builder.add(InstantiateScopeConstraint(instantiatedStruct, structDeclaration))
      TypeApplication(StructType(instantiatedStruct), Seq(typeArgument))
    })
    val instantiatedStructDeclaration: Declaration = structType.function.asInstanceOf[StructType].declaration
    val structScope = builder.declaredScopeVariable(instantiatedStructDeclaration)
    builder.reference(structName, this, parentScope, instantiatedStructDeclaration)
    builder.typesAreEqual(_type, structType)
    fieldInitializers.foreach(value => value.constraints(builder, structScope, parentScope))
  }

  override def evaluate(machine: Machine): MachineType = {
    val structType = machine.resolveStruct(structName)
    val instantiatedStructType = languageTypeArgument.fold(structType)(t => structType.instantiate(structType.parameterType.get, t.evaluate(machine)))

    fieldInitializers.foreach(fieldInit => {
      val fieldType = instantiatedStructType.resolve(fieldInit.fieldName)
      machine.assertSubType(fieldInit.value.evaluate(machine), fieldType)
    })
    instantiatedStructType
  }
}
